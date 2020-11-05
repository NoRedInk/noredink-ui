import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import qualified GHC.IO.Encoding
import qualified System.IO

main :: IO ()
main = do
  shakeArgs
    shakeOptions
      { shakeFiles = "_build",
        shakeThreads = 15
      }
    $ do
      phony "public" $ need ["_build/asd"]

      "_build/asd" %> \out -> do
        alwaysRerun
        writeFileChanged out "hi"