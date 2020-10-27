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
