module IdentityOutput where

import Middleman.Types (MiddlemanResponse)

transform :: MiddlemanResponse -> IO MiddlemanResponse
transform = pure
