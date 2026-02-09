module Middleman.Proxy
  ( ProxyError (..)
  , forwardRequest
  , substituteParams
  ) where

import Control.Exception (SomeException, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.CaseInsensitive as CI
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)
  , PathParams
  , RouteConfig (..)
  , ServiceConfig (..)
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types (Header)

-- | Errors that can occur during proxy forwarding
data ProxyError
  = ProxyConnectionError Text
  | ProxyHttpError Text
  deriving (Show, Eq)

-- | Substitute {param} placeholders in a template with captured values
substituteParams :: PathParams -> Text -> Text
substituteParams params template =
  foldl' (\t (name, val) -> T.replace ("{" <> name <> "}") val t) template params

-- | Forward a request to the target service
forwardRequest
  :: HTTP.Manager
  -> ServiceConfig
  -> RouteConfig
  -> PathParams
  -> MiddlemanRequest
  -> IO (Either ProxyError MiddlemanResponse)
forwardRequest manager svc route params req = do
  let targetPath = substituteParams params (routeTargetPath route)
      targetUrl = unpack (serviceBaseUrl svc) <> unpack targetPath
  result <- try @SomeException $ do
    initReq <- HTTP.parseRequest targetUrl
    let authHeaders = maybe [] buildAuthHeaders (serviceAuth svc)
        -- Forward original headers, filtering hop-by-hop and framing headers
        -- that http-client will set itself based on the actual request body
        filteredHeaders = filter (\(name, _) -> name `notElem` stripRequestHeaders) (mrHeaders req)
        httpReq =
          initReq
            { HTTP.method = mrMethod req
            , HTTP.requestHeaders = filteredHeaders <> authHeaders
            , HTTP.requestBody = HTTP.RequestBodyBS (mrBody req)
            , HTTP.queryString = mrQueryString req
            }
    HTTP.httpLbs httpReq manager
  case result of
    Left err ->
      pure (Left (ProxyConnectionError (pack (show err))))
    Right resp ->
      let respHeaders = filter (\(name, _) -> name `notElem` stripResponseHeaders) (HTTP.responseHeaders resp)
      in pure
        ( Right
            ( MiddlemanResponse
                { mresStatus = HTTP.responseStatus resp
                , mresHeaders = respHeaders
                , mresBody = LBS.toStrict (HTTP.responseBody resp)
                }
            )
        )

-- | Headers to strip from the agent's request before forwarding.
-- These are hop-by-hop or body-framing headers that http-client sets itself
-- based on the actual request body; forwarding the originals causes conflicts
-- (duplicate Content-Length, chunked framing on a non-chunked body, etc.).
stripRequestHeaders :: [CI.CI BS.ByteString]
stripRequestHeaders =
  [ "Host"
  , "Content-Length"
  , "Transfer-Encoding"
  ]

-- | Headers to strip from the target's response before returning to the agent.
-- Warp sets its own framing headers based on the response body we hand it.
stripResponseHeaders :: [CI.CI BS.ByteString]
stripResponseHeaders =
  [ "Content-Length"
  , "Transfer-Encoding"
  ]

-- | Build auth headers based on the auth config
buildAuthHeaders :: AuthConfig -> [Header]
buildAuthHeaders AuthConfig{..} =
  case authType of
    Bearer ->
      [("Authorization", "Bearer " <> encodeUtf8 authToken)]
    BasicAuth ->
      [("Authorization", "Basic " <> encodeUtf8 authToken)]
    HeaderAuth ->
      case authHeaderName of
        Just name -> [(CI.mk (encodeUtf8 name), encodeUtf8 authToken)]
        Nothing -> [("Authorization", encodeUtf8 authToken)]
