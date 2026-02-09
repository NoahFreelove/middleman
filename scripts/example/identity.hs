module Identity where

import Middleman.Types (MiddlemanRequest, MiddlemanResponse)

transform :: MiddlemanRequest -> IO MiddlemanRequest
transform = pure
