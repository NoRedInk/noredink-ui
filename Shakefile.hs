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
        shakeChange = ChangeModtimeAndDigest
      }
    $ do
      phony "bla" $ need ["single-dep"]

      "single-dep" %> \out -> do
        putNormal "this rule executed!"
        need ["elm.json"]
        writeFile' out "hooray."

      "all-elms" %> \out -> do
        putNormal "this rule executed!"
        allElms <- getDirectoryFiles "" ["//*.elm"]
        writeFile' out $ show allElms

      "all-elm-contents" %> \out -> do
        putNormal "this rule executed!"
        allElms <- getDirectoryFiles "" ["//*.elm"]
        need allElms
        writeFile' out "hooray."

      "dependencies" %> \out -> do
        putNormal "this rule executed!"
        elmFile <- readFile' "bla.elm"
        need [elmFile]
        writeFile' out "hooray."

      "unsafe-dependencies" %> \out -> do
        putNormal "this rule executed!"
        elmFile <- liftIO $ readFile "bla.elm"
        need [elmFile]
        writeFile' out "hooray."

      "dirfiles" %> \out -> do
        putNormal "this rule executed!"
        allElms <- liftIO $ getDirectoryFilesIO "" ["//*.elm"]
        need allElms
        writeFile' out "hooray."
