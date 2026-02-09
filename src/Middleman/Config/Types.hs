module Middleman.Config.Types
  ( ConfigError (..)
  , renderConfigError
  ) where

import Data.Text (Text, pack)

-- | Errors that can occur during config loading/parsing/validation
data ConfigError
  = ConfigFileNotFound FilePath
  | ConfigParseError Text
  | ConfigValidationError Text
  deriving (Show, Eq)

-- | Render a ConfigError to a human-readable message
renderConfigError :: ConfigError -> Text
renderConfigError (ConfigFileNotFound path) =
  "Config file not found: " <> pack path
renderConfigError (ConfigParseError msg) =
  "Config parse error: " <> msg
renderConfigError (ConfigValidationError msg) =
  "Config validation error: " <> msg
