import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main =
  -- TODO: better shake options. Parallelism, hash changes.
  shakeArgs
    shakeOptions
      { shakeFiles = "_build",
        shakeLintIgnore = ["node_modules/**/*", ".git/**/*"],
        shakeThreads = 0,
        shakeChange = ChangeModtimeAndDigest
      }
    $ do
      -- things that should be kept under version control
      "tests/elm-verify-examples.json" %> \out -> do
        need ["elm.json"]
        Stdout newConfig <- cmd "jq" "--indent" "4" ["{ root: \"../src\", tests: .[\"exposed-modules\"] }"] "elm.json"
        writeFileChanged out newConfig

      -- temporary files, used to produce CI reports
      "log/format.txt" %> \out -> do
        let placesToLook = ["src", "tests", "styleguide-app"]
        elmFiles <- getDirectoryFiles "." (map (\place -> place </> "**" </> "*.elm") placesToLook)
        need elmFiles
        Stdout report <- cmd "elm-format" "--validate" placesToLook
        writeFileChanged out report

      "log/percy-tests.txt" %> \out -> do
        percyToken <- getEnv "PERCY_TOKEN"
        case percyToken of
          Nothing -> do
            writeFileChanged out "Skipped running Percy tests, PERCY_TOKEN not set."
          Just _ -> do
            need ["log/npm-install.txt"]
            Stdout report <- cmd "script/percy-tests.sh"
            writeFileChanged out report

      "log/axe-report.json" %> \out -> do
        need ["log/npm-install.txt", "script/run-axe.sh", "script/axe-puppeteer.js"]
        Stdout report <- cmd "script/run-axe.sh"
        writeFileChanged out report

      "log/axe-report.txt" %> \out -> do
        need ["log/axe-report.json", "script/format-axe-report.sh", "script/axe-report.jq"]
        Stdout report <- cmd "script/format-axe-report.sh" "log/axe-report.json"
        writeFileChanged out report

      -- dev deps we get dynamically instead of from Nix (frowny face)
      "log/npm-install.txt" %> \out -> do
        -- npm looks in some unrelated files for whatever reason. We mark
        -- them as used here to avoid getting linter errors.
        needed ["README.md"]

        -- now that we've satisfied the linter, let's build.
        need ["package.json", "package-lock.json"]
        Stdout report <- cmd "npm install"
        writeFileChanged out report

      -----------------
      -- DANGER ZONE --
      -----------------
      --
      -- everything below this line has been translated extremely literally from
      -- the old Makefile, and probably has serious issues. As I make sure all
      -- the dependencies are actually tracked and satisfied, they'll move above
      -- this line.

      phony "clean" $ do
        removeFilesAfter "elm-stuff" ["//*"]
        removeFilesAfter "log" ["//*"]
        removeFilesAfter "node_modules" ["//*"]
        removeFilesAfter "public" ["//*"]
        removeFilesAfter "styleguide-app" ["elm.js", "bundle.js", "elm-stuff"]

      phony "test" $ do
        need ["log/node_modules.txt", "tests/elm-verify-examples.json"]
        cmd_ "npx" "elm-verify-examples"
        cmd_ "npx" "elm-test"
        need ["log/axe-report.txt", "log/percy-tests.txt", "log/deprecated-imports-report.txt"]

      phony "ci" $ do
        need ["log/check-exposed.txt", "test", "log/format.txt", "log/documentation.json", "public"]

      -- deprecated imports
      --
      -- still need something to error when they fail (i.e. when running the
      -- `check` subcommand)
      "log/deprecated-imports-report.txt" %> \out -> do
        need ["script/deprecated-imports.py"]
        Stdout report <- cmd "script/deprecated-imports.py report"
        writeFileChanged out report

      "log/deprecated-imports.csv" %> \out -> do
        need ["script/deprecated-imports.py"]
        cmd_ "script/deprecated-imports.py" "--imports-file" out "update"

      "log/check-exposed.txt" %> \out -> do
        need ["script/check-exposed.py"] -- TODO: need Elm files, elm JSON
        Stdout report <- cmd "script/check-exposed.py"
        writeFileChanged out report

      "log/documentation.json" %> \out -> do
        need ["log/node_modules.txt"]
        cmd_ "elm" "make" "--docs" out

      "styleguide-app/bundle.js" %> \out -> do
        need ["lib/index.js", "styleguide-app/manifest.js", "log/node_modules.txt"]
        cmd_ "npx" "browserify" "--entry" "styleguide-app/manifest.js" "--outfile" out

      "styleguide-app/elm.js" %> \out -> do
        need ["styleguide-app/bundle.js"] -- ported directly from Make... why is this needed?
        elmSources <- getDirectoryFiles "." ["styleguide/**/*.elm", "src/**/*.elm"]
        need elmSources
        cmd_ (Cwd "styleguide-app") "npx" "elm" "make" "Main.elm" "--output" (takeFileName out)

      -- public folder for styleguide
      "public/**/*" %> \out ->
        copyFileChanged (replaceDirectory1 out "styleguide-app") out

      "log/public.txt" %> \out -> do
        styleguideAssets <- getDirectoryFiles ("styleguide-app" </> "assets") ["**/*"]
        need
          ( ["public/index.html", "public/elm.js", "public/bundle.js"]
              ++ map (("public" </> "assets") </>) styleguideAssets
          )
        writeFileChanged out "done"

      phony "public" $ need ["log/public.txt"]
