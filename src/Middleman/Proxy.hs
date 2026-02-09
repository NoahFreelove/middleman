module Middleman.Proxy
  ( ProxyError (..)
  , forwardRequest
  , substituteParams
  ) where

import Control.Exception (SomeException, try)
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
    let authHeaders = buildAuthHeaders (serviceAuth svc)
        -- Forward original headers, but filter out Host (will be set by http-client)
        filteredHeaders = filter (\(name, _) -> name /= "Host") (mrHeaders req)
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
      pure
        ( Right
            ( MiddlemanResponse
                { mresStatus = HTTP.responseStatus resp
                , mresHeaders = HTTP.responseHeaders resp
                , mresBody = LBS.toStrict (HTTP.responseBody resp)
                }
            )
        )

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
