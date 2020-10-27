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
