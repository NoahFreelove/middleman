module Middleman.Script
  ( ScriptError (..)
  , runInputScript
  , runOutputScript
  ) where

import Middleman.Script.Haskell (runHaskellInputScript, runHaskellOutputScript)
import Middleman.Script.Python (runPythonInputScript, runPythonOutputScript)
import Middleman.Script.Types (ScriptError (..))
import Middleman.Types
  ( MiddlemanRequest
  , MiddlemanResponse
  , ScriptLanguage (..)
  , ScriptRef (..)
  )

-- | Run an input script to transform a request
runInputScript :: ScriptRef -> MiddlemanRequest -> IO (Either ScriptError MiddlemanRequest)
runInputScript ref req =
  case scriptLanguage ref of
    Haskell -> runHaskellInputScript (scriptPath ref) req
    Python -> runPythonInputScript (scriptPath ref) req

-- | Run an output script to transform a response
runOutputScript :: ScriptRef -> MiddlemanResponse -> IO (Either ScriptError MiddlemanResponse)
runOutputScript ref resp =
  case scriptLanguage ref of
    Haskell -> runHaskellOutputScript (scriptPath ref) resp
    Python -> runPythonOutputScript (scriptPath ref) resp
