module Middleman.Script.Types
  ( ScriptError (..)
  ) where

import Data.Text (Text)
import Middleman.Types (ScriptLanguage)

-- | Errors that can occur during script execution
data ScriptError
  = ScriptLoadError Text
  | ScriptRuntimeError Text
  | UnsupportedLanguage ScriptLanguage
  deriving (Show, Eq)
