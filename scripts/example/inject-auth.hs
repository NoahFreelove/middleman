module InjectAuth where

import Middleman.Types (MiddlemanRequest (..))
import System.Environment (lookupEnv)
import Data.Text.Encoding (encodeUtf8)
import Data.Text (pack)

transform :: MiddlemanRequest -> IO MiddlemanRequest
transform req = do
  mToken <- lookupEnv "MY_API_TOKEN"
  case mToken of
    Nothing -> pure req
    Just token ->
      let authHeader = ("Authorization", "Bearer " <> encodeUtf8 (pack token))
      in pure req { mrHeaders = mrHeaders req ++ [authHeader] }
