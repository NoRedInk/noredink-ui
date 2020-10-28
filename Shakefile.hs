import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions {shakeFiles = "_build"} $ do
  "tests/elm-verify-examples.json" %> \out -> do
    need ["elm.json"]
    Stdout newConfig <- cmd "jq" "--indent" "4" ["{ root: \"../src\", tests: .[\"exposed-modules\"] }"] "elm.json"
    writeFileChanged out newConfig

  "log/axe-report.json" %> \out -> do
    need ["public", "script/run-axe.sh", "script/axe-puppeteer.js"]
    Stdout report <- cmd "script/run-axe.sh"
    writeFileChanged out report

  "log/axe-report" %> \out -> do
    need ["log/axe-report.json", "script/format-axe-report.sh", "script/axe-report.jq"]
    Stdout report <- cmd "script/format-axe-report.sh" "log/axe-report.json"
    writeFileChanged out report

  "log/percy-tests" %> \out -> do
    Stdout report <- cmd "script/percy-tests.sh"
    writeFileChanged out report

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
