module Middleman.Script.Python
  ( runPythonInputScript
  , runPythonOutputScript
  ) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.Text (Text)
import qualified Data.Text as T
import Middleman.Script.Types (ScriptError (..))
import Middleman.Types (MiddlemanRequest, MiddlemanResponse)
import System.Directory (doesFileExist)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Run a Python input script to transform a request.
-- The script must define: def transform(data): ... -> dict
runPythonInputScript :: FilePath -> MiddlemanRequest -> IO (Either ScriptError MiddlemanRequest)
runPythonInputScript = runPythonScript

-- | Run a Python output script to transform a response.
-- The script must define: def transform(data): ... -> dict
runPythonOutputScript :: FilePath -> MiddlemanResponse -> IO (Either ScriptError MiddlemanResponse)
runPythonOutputScript = runPythonScript

-- | Generic Python script runner. Encodes the input as JSON, pipes it through
-- a Python subprocess that calls the script's transform() function, and decodes
-- the JSON output.
runPythonScript :: (ToJSON a, FromJSON a) => FilePath -> a -> IO (Either ScriptError a)
runPythonScript path input = do
  exists <- doesFileExist path
  if not exists
    then pure (Left (ScriptLoadError ("Script file not found: " <> T.pack path)))
    else do
      let bootstrap = pythonBootstrap path
          cp = (proc "python3" ["-c", bootstrap])
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
      (Just hIn, Just hOut, Just hErr, ph) <- createProcess cp
      LBS.hPut hIn (encode input)
      hClose hIn
      output <- LBS.hGetContents hOut
      errOutput <- LBS.hGetContents hErr
      exitCode <- waitForProcess ph
      case exitCode of
        ExitFailure code ->
          pure (Left (ScriptRuntimeError (formatError path code errOutput)))
        ExitSuccess ->
          case eitherDecode output of
            Left decodeErr ->
              pure (Left (ScriptRuntimeError ("Failed to decode script output: " <> T.pack decodeErr)))
            Right result ->
              pure (Right result)

-- | Generate the Python bootstrap code that loads and runs the user's script.
pythonBootstrap :: FilePath -> String
pythonBootstrap scriptPath = unlines
  [ "import sys, json, importlib.util"
  , "spec = importlib.util.spec_from_file_location('_script', " <> show scriptPath <> ")"
  , "mod = importlib.util.module_from_spec(spec)"
  , "spec.loader.exec_module(mod)"
  , "data = json.load(sys.stdin)"
  , "result = mod.transform(data)"
  , "json.dump(result, sys.stdout)"
  ]

-- | Format a Python process error for the ScriptRuntimeError.
formatError :: FilePath -> Int -> LBS.ByteString -> Text
formatError path code stderr =
  "Python script " <> T.pack path <> " failed with exit code " <> T.pack (show code)
  <> ": " <> T.pack (LBS8.unpack stderr)
