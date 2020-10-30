import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main =
  shakeArgs
    shakeOptions
      { shakeFiles = "_build",
        shakeThreads = 0,
        shakeChange = ChangeModtimeAndDigest,
        -- we ignore a lot of generated/downloaded dependency files so
        -- that the output of `shake --lint-fsatrace` is usable. There
        -- are probably a few untracked dependencies due to these ignores
        -- (in particular relying on scripts in `node_modules`) but the
        -- additional benefits are marginal compared to the effort required
        -- to get everything 100% buttoned down. Long term, it'd be better to
        -- move node dependencies into nix (either by using proper packages
        -- where available or npm2nix where not.)
        shakeLintIgnore =
          [ "node_modules/**/*",
            "elm-stuff/**/*",
            "styleguide-app/elm-stuff/**/*"
          ]
      }
    $ do
      -- phonies. These provide a nice public API for using shake (`shake
      -- clean`, `shake test`, etc.)
      phony "clean" $ do
        removeFilesAfter "elm-stuff" ["//*"]
        removeFilesAfter "log" ["//*"]
        removeFilesAfter "node_modules" ["//*"]
        removeFilesAfter "public" ["//*"]
        removeFilesAfter "styleguide-app" ["elm.js", "bundle.js", "elm-stuff"]

      phony "public" $ need ["log/public.txt"]

      phony "test" $ do
        need
          [ "log/npm-install.txt",
            "tests/elm-verify-examples.json",
            "log/elm-verify-examples.txt",
            "log/elm-test.txt",
            "log/axe-report.txt",
            "log/percy-tests.txt",
            "log/deprecated-imports-report.txt",
            "log/check-exposed.txt",
            "log/format.txt",
            "log/documentation.json"
          ]

      phony "ci" $ do need ["test", "public"]

      -- things that should be kept under version control
      "tests/elm-verify-examples.json" %> \out -> do
        need ["elm.json"]
        cmd (FileStdout out) "jq" "--indent" "4" ["{ root: \"../src\", tests: .[\"exposed-modules\"] }"] "elm.json"

      "script/deprecated-imports.csv" %> \out -> do
        getEnv "DEPRECATED_MODULES"
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm", "tests/**/*.elm"]
        need (["elm.json", "script/deprecated-imports.py"] ++ elmFiles)
        cmd_ "script/deprecated-imports.py" "--imports-file" out "update"

      -- temporary files, used to produce CI reports
      "log/elm-test.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["tests/**/*.elm"]
        -- I'm not sure why elm-test needs package.json, but fsatracing it
        -- reveals the dep, so in it goes!
        need (["package.json", "elm.json"] ++ elmFiles)
        cmd (FileStdout out) "elm-test"

      "log/elm-verify-examples.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need (["tests/elm-verify-examples.json"] ++ elmFiles)
        cmd (FileStdout out) "elm-verify-examples"

      "log/format.txt" %> \out -> do
        let placesToLook = ["src", "tests", "styleguide-app"]
        elmFiles <- getDirectoryFiles "." (map (\place -> place </> "**" </> "*.elm") placesToLook)
        need elmFiles
        cmd (FileStdout out) "elm-format" "--validate" placesToLook

      "log/percy-tests.txt" %> \out -> do
        percyToken <- getEnv "PERCY_TOKEN"
        case percyToken of
          Nothing -> do
            writeFileChanged out "Skipped running Percy tests, PERCY_TOKEN not set."
          Just _ -> do
            need ["log/npm-install.txt"]
            cmd (FileStdout out) "script/percy-tests.js"

      "log/axe-report.json" %> \out -> do
        need ["log/npm-install.txt", "script/run-axe.sh", "script/axe-puppeteer.js"]
        cmd (FileStdout out) "script/run-axe.sh"

      "log/axe-report.txt" %> \out -> do
        need ["log/axe-report.json", "script/format-axe-report.sh", "script/axe-report.jq"]
        cmd (FileStdout out) "script/format-axe-report.sh" "log/axe-report.json"

      "log/deprecated-imports-report.txt" %> \out -> do
        getEnv "DEPRECATED_MODULES"
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm", "tests/**/*.elm"]
        need (["elm.json", "script/deprecated-imports.py"] ++ elmFiles)
        cmd (FileStdout out) "script/deprecated-imports.py" "check"

      "log/check-exposed.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need (["elm.json", "script/check-exposed.py"] ++ elmFiles)
        cmd (FileStdout out) "script/check-exposed.py"

      "log/documentation.json" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need elmFiles
        cmd_ "elm" "make" "--docs" out

      "public/bundle.js" %> \out -> do
        libJsFiles <- getDirectoryFiles "." ["lib/**/*.js"]
        need (["package.json", "lib/index.js", "styleguide-app/manifest.js", "log/npm-install.txt"] ++ libJsFiles)
        cmd_ "./node_modules/.bin/browserify" "--entry" "styleguide-app/manifest.js" "--outfile" out

      "public/elm.js" %> \out -> do
        elmSources <- getDirectoryFiles "." ["styleguide-app/**/*.elm", "src/**/*.elm"]
        need elmSources
        cmd_ (Cwd "styleguide-app") "elm" "make" "Main.elm" "--output" (".." </> out)

      "public/**/*" %> \out ->
        copyFileChanged (replaceDirectory1 out "styleguide-app") out

      "log/public.txt" %> \out -> do
        styleguideAssets <- getDirectoryFiles ("styleguide-app" </> "assets") ["**/*"]
        need
          ( ["public/index.html", "public/elm.js", "public/bundle.js"]
              ++ map (("public" </> "assets") </>) styleguideAssets
          )
        writeFileChanged out "built styleguide app successfully"

      -- dev deps we get dynamically instead of from Nix (frowny face)
      "log/npm-install.txt" %> \out -> do
        -- npm looks in some unrelated files for whatever reason. We mark
        -- them as used here to avoid getting linter errors.
        gitHeads <- getDirectoryFiles "." [".git/refs/heads/*"]
        needed (["README.md", ".git/HEAD"] ++ gitHeads)

        -- now that we've satisfied the linter, let's build.
        need ["package.json", "package-lock.json"]
        cmd (FileStdout out) (FileStderr out) "npm" "install"
