import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified GHC.IO.Encoding
import qualified System.IO

main :: IO ()
main = do
  -- Work around `hGetContents: invalid argument (invalid byte sequence)` bug on
  -- Nix: https://github.com/dhall-lang/dhall-haskell/issues/865
  GHC.IO.Encoding.setLocaleEncoding System.IO.utf8
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
            "styleguide/elm-stuff/**/*"
          ]
      }
    $ do
      want ["test"]
      -- phonies. These provide a nice public API for using shake (`shake
      -- clean`, `shake test`, etc.)
      phony "clean" $ do
        removeFilesAfter "elm-stuff" ["//*"]
        removeFilesAfter "log" ["//*"]
        removeFilesAfter "node_modules" ["//*"]
        removeFilesAfter "public" ["//*"]
        removeFilesAfter "styleguide" ["elm.js", "bundle.js", "elm-stuff"]

      phony "public" $ need ["log/public.txt"]

      phony "test" $ do
        need
          [ "log/npm-install.txt",
            "tests/elm-verify-examples.json",
            "log/elm-verify-examples.txt",
            "log/elm-test.txt",
            "log/elm-test-styleguide.txt",
            "log/elm-review.txt",
            "log/elm-review-styleguide.txt",
            "log/puppeteer-tests.txt",
            "log/forbidden-imports-report.txt",
            "log/check-exposed.txt",
            "log/format.txt",
            "log/documentation.json"
          ]

      phony "ci" $ do need ["test", "public"]

      -- things that should be kept under version control
      "tests/elm-verify-examples.json" %> \out -> do
        need ["elm.json"]
        cmd (WithStdout True) (FileStdout out) "jq" "--indent" "4" ["{ root: \"../src\", tests: .[\"exposed-modules\"] }"] "elm.json"

      "deprecated-modules.csv" %> \out -> do
        need ["elm.json", "script/deprecated-modules-csv.py"]
        cmd (WithStdout True) (FileStdout out) "script/deprecated-modules-csv.py"

      "forbidden-imports.toml" %> \out -> do
        -- we always want to consume our own published deprecated modules list
        -- so that we're not presenting outdated stuff in the styleguide! This
        -- file can change separately from the CSV, but should always have at
        -- least the deprecated modules in it.
        need ["deprecated-modules.csv"]
        cmd "elm-forbid-import" "forbid-from-csv" "deprecated-modules.csv"

      -- temporary files, used to produce CI reports
      "log/elm-test.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["tests/**/*.elm"]
        -- I'm not sure why elm-test needs package.json, but fsatracing it
        -- reveals the dep, so in it goes!
        need (["package.json", "elm.json"] ++ elmFiles)
        cmd (WithStdout True) (FileStdout out) "elm-test"

      "log/elm-test-styleguide.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["styleguide/tests/**/*.elm"]
        need (["package.json", "styleguide/elm.json"] ++ elmFiles)
        cmd (Cwd "styleguide") (WithStdout True) (FileStdout out) "elm-test"

      "log/elm-review.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm", "tests/**/*.elm"]
        need (["package.json", "elm.json"] ++ elmFiles)
        cmd (WithStdout True) (FileStdout out) "elm-review"

      "log/elm-review-styleguide.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["styleguide/**/*.elm", "styleguide-app/**/*.elm"]
        need (["package.json", "styleguide/elm.json"] ++ elmFiles)
        cmd (Cwd "styleguide") (WithStdout True) (FileStdout out) "elm-review"

      "log/elm-verify-examples.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need (["tests/elm-verify-examples.json"] ++ elmFiles)
        cmd (WithStdout True) (FileStdout out) "elm-verify-examples"

      "log/format.txt" %> \out -> do
        need ["log/elm-format.txt", "log/prettier.txt"]
        writeFileChanged out "formatting checks passed"

      "log/elm-format.txt" %> \out -> do
        let placesToLook = ["src", "tests", "styleguide", "styleguide-app"]
        elmFiles <- getDirectoryFiles "." (map (\place -> place </> "**" </> "*.elm") placesToLook)
        need elmFiles
        cmd (WithStdout True) (FileStdout out) "elm-format" "--validate" placesToLook

      "log/prettier.txt" %> \out -> do
        (Stdout trackedFilesOut) <- cmd "git" "ls-files"
        let trackedFiles = lines trackedFilesOut
        let jsFiles = filter (\name -> takeExtension name == ".js") trackedFiles
        need ("log/npm-install.txt" : jsFiles)

        cmd (WithStdout True) (FileStdout out) "./node_modules/.bin/prettier" "--check" jsFiles

      "log/puppeteer-tests.txt" %> \out -> do
        percyToken <- getEnv "PERCY_TOKEN"
        case percyToken of
          Nothing -> do
            writeFileChanged out "PERCY_TOKEN not set, so skipping visual diff testing."
            need ["log/npm-install.txt", "log/public.txt"]
            cmd (WithStdout True) (FileStdout out) "script/puppeteer-tests-no-percy.sh"
          Just _ -> do
            need ["log/npm-install.txt", "log/public.txt"]
            cmd (WithStdout True) (FileStdout out) "script/puppeteer-tests-percy.sh"

      "log/forbidden-imports-report.txt" %> \out -> do
        need ["forbidden-imports.toml"]
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm", "tests/**/*.elm"]
        need (["forbidden-imports.toml"] ++ elmFiles)
        cmd (WithStdout True) (FileStdout out) "elm-forbid-import" "check"

      "log/check-exposed.txt" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need (["elm.json", "script/check-exposed.py"] ++ elmFiles)
        cmd (WithStdout True) (FileStdout out) "script/check-exposed.py"

      "log/documentation.json" %> \out -> do
        elmFiles <- getDirectoryFiles "." ["src/**/*.elm"]
        need elmFiles
        cmd_ "elm" "make" "--docs" out

      "public/bundle.js" %> \out -> do
        libJsFiles <- getDirectoryFiles "." ["lib/**/*.js"]
        need (["package.json", "lib/index.js", "styleguide/manifest.js", "log/npm-install.txt"] ++ libJsFiles)
        cmd_ "./node_modules/.bin/browserify" "--entry" "styleguide/manifest.js" "--outfile" out

      "public/elm.js" %> \out -> do
        elmSources <- getDirectoryFiles "." ["styleguide-app/**/*.elm", "src/**/*.elm"]
        need elmSources
        cmd_ (Cwd "styleguide") "elm" "make" "Main.elm" "--output" (".." </> out)

      "public/package.json" %> \out -> do
        copyFileChanged "elm.json" out

      "public/application.json" %> \out -> do
        copyFileChanged "styleguide/elm.json" out

      "public/**/*" %> \out ->
        copyFileChanged (replaceDirectory1 out "styleguide") out

      "log/public.txt" %> \out -> do
        need (["public/index.html", "public/elm.js", "public/bundle.js", "public/package.json", "public/application.json"])
        writeFileChanged out "built styleguide app successfully"

      -- dev deps we get dynamically instead of from Nix (frowny face)
      "log/npm-install.txt" %> \out -> do
        -- npm looks in some unrelated files for whatever reason. We mark
        -- them as used here to avoid getting linter errors.
        gitHeads <- getDirectoryFiles "." [".git/refs/heads/*"]
        trackRead (["README.md", ".git/HEAD"] ++ gitHeads)

        -- now that we've satisfied the linter, let's build.
        need ["package.json", "package-lock.json"]
        cmd (WithStdout True) (FileStdout out) (FileStderr out) "npm" "install"
