module Middleman
  ( version
  , startServer
  , loadConfig
  , module Middleman.Types
  ) where

import Data.Text (Text)
import Middleman.Config (loadConfig)
import Middleman.Server (startServer)
import Middleman.Types

version :: Text
version = "0.1.0.0"
