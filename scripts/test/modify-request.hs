module ModifyRequest where

import Middleman.Types (MiddlemanRequest (..))

transform :: MiddlemanRequest -> IO MiddlemanRequest
transform req = pure req { mrHeaders = mrHeaders req ++ [("X-Modified-By", "haskell-script")] }
