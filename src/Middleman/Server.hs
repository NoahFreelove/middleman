module Middleman.Server
  ( startServer
  , makeApp
  ) where

import Control.Exception (finally)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Middleman.Logging
  ( Logger
  , closeLogger
  , logError
  , logInfo
  , logRequest
  , logResponse
  , newLogger
  )
import Middleman.Pipeline (PipelineError (..), runPipeline)
import Middleman.Proxy (ProxyError (..), substituteParams)
import Middleman.Router (MatchResult (..), matchRoute)
import Middleman.Script (ScriptError (..))
import Middleman.Types
  ( GlobalConfig (..)
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)
  , PathParams
  , RouteConfig (..)
  , ServiceConfig (..)
  )
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types
  ( forbidden403
  , hContentType
  , internalServerError500
  , methodNotAllowed405
  , notFound404
  , ok200
  , statusCode
  , unauthorized401
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Start the middleman server
startServer :: GlobalConfig -> IO ()
startServer cfg = do
  logger <- newLogger
  manager <- HTTP.newManager TLS.tlsManagerSettings
  logInfo logger ("middleman starting on port " <> pack (show (globalPort cfg)))
  let settings =
        Warp.setPort (globalPort cfg) $
        Warp.setOnException (\_ ex ->
          logError logger ("Unhandled exception: " <> pack (show ex))
        ) $
        Warp.setGracefulShutdownTimeout (Just 5) $
        Warp.defaultSettings
  Warp.runSettings settings (makeApp logger manager cfg)
    `finally` closeLogger logger

-- | Build a WAI Application from config
makeApp :: Logger -> HTTP.Manager -> GlobalConfig -> Wai.Application
makeApp logger manager cfg waiReq respond = do
  let path = normalizePath (decodeUtf8 (Wai.rawPathInfo waiReq))
      method = Wai.requestMethod waiReq
  logRequest logger method path
  if not (checkAgentAuth cfg waiReq)
    then respond $ Wai.responseLBS unauthorized401
      [(hContentType, "application/json")]
      "{\"error\":\"Unauthorized\"}"
    else case path of
      "/" -> respond $ serveIndex cfg
      "/index" -> respond $ serveIndex cfg
      _ -> handleRoute logger manager cfg path method waiReq respond

-- | Check incoming request against the global auth token (if configured)
checkAgentAuth :: GlobalConfig -> Wai.Request -> Bool
checkAgentAuth cfg waiReq =
  case globalAuthToken cfg of
    Nothing -> True  -- open access, no token required
    Just token ->
      let authHeader = lookup "Authorization" (Wai.requestHeaders waiReq)
          expected = "Bearer " <> encodeUtf8 token
      in authHeader == Just expected

-- | Serve the index page showing available routes
serveIndex :: GlobalConfig -> Wai.Response
serveIndex cfg =
  Wai.responseLBS ok200
    [(hContentType, "application/json")]
    (Aeson.encode (indexPayload cfg))

-- | Build the JSON index payload from config
indexPayload :: GlobalConfig -> Aeson.Value
indexPayload cfg =
  Aeson.object
    [ "services" .= map serviceIndex (globalServices cfg) ]
  where
    serviceIndex :: ServiceConfig -> Aeson.Value
    serviceIndex svc =
      Aeson.object $
        [ "name" .= serviceName svc
        , "routes" .= map routeIndex (serviceRoutes svc)
        ] ++
        [ "allowedMethods" .= map decodeUtf8 (allowedMethods svc)
        | not (null (allowedMethods svc))
        ] ++
        [ "invert" .= True
        | serviceInvert svc
        ]

    routeIndex :: RouteConfig -> Aeson.Value
    routeIndex route =
      Aeson.object
        [ "path" .= routePath route
        , "method" .= decodeUtf8 (routeMethod route)
        ]

-- | Handle a normal (non-index) request
handleRoute :: Logger -> HTTP.Manager -> GlobalConfig -> Text -> BS.ByteString -> Wai.Application
handleRoute logger manager cfg path method waiReq respond =
  case matchRoute cfg path method of
    NoRouteFound -> do
      logError logger ("No route found: " <> path)
      respond $ Wai.responseLBS notFound404
        [(hContentType, "application/json")]
        "{\"error\":\"Not found\"}"
    MethodNotAllowed p -> do
      logError logger ("Method not allowed: " <> decodeUtf8 method <> " " <> p)
      respond $ Wai.responseLBS methodNotAllowed405
        [(hContentType, "application/json")]
        "{\"error\":\"Method not allowed\"}"
    RouteDenied p -> do
      logError logger ("Route denied: " <> decodeUtf8 method <> " " <> p)
      respond $ Wai.responseLBS forbidden403
        [(hContentType, "application/json")]
        "{\"error\":\"Forbidden\"}"
    RouteMatched svc route params -> do
      mReq <- waiToMiddleman waiReq
      let enrichedReq = enrichRequest mReq route params
      result <- runPipeline manager cfg svc route params enrichedReq
      case result of
        Left err -> do
          logError logger ("Pipeline error: " <> pack (show err))
          respond $ Wai.responseLBS internalServerError500
            [(hContentType, "application/json")]
            (LBS.fromStrict (renderPipelineError err))
        Right mResp -> do
          logResponse logger (statusCode (mresStatus mResp)) path
          respond $ middlemanToWai mResp

-- | Convert a WAI Request to a MiddlemanRequest
waiToMiddleman :: Wai.Request -> IO MiddlemanRequest
waiToMiddleman waiReq = do
  body <- Wai.consumeRequestBodyStrict waiReq
  pure MiddlemanRequest
    { mrMethod = Wai.requestMethod waiReq
    , mrPath = decodeUtf8 (Wai.rawPathInfo waiReq)
    , mrHeaders = Wai.requestHeaders waiReq
    , mrBody = LBS.toStrict body
    , mrQueryString = Wai.rawQueryString waiReq
    , mrRoutePath = ""
    , mrTargetPath = ""
    }

-- | Enrich a MiddlemanRequest with route context (read-only for scripts)
enrichRequest :: MiddlemanRequest -> RouteConfig -> PathParams -> MiddlemanRequest
enrichRequest req route params =
  req
    { mrRoutePath = routePath route
    , mrTargetPath = substituteParams params (routeTargetPath route)
    }

-- | Convert a MiddlemanResponse to a WAI Response
middlemanToWai :: MiddlemanResponse -> Wai.Response
middlemanToWai MiddlemanResponse{..} =
  Wai.responseLBS mresStatus mresHeaders (LBS.fromStrict mresBody)

-- | Render a pipeline error as a JSON byte string
renderPipelineError :: PipelineError -> BS.ByteString
renderPipelineError (PipelineScriptError (ScriptLoadError msg)) =
  "{\"error\":\"Script load error: " <> encodeUtf8 msg <> "\"}"
renderPipelineError (PipelineScriptError (ScriptRuntimeError msg)) =
  "{\"error\":\"Script runtime error: " <> encodeUtf8 msg <> "\"}"
renderPipelineError (PipelineScriptError (UnsupportedLanguage lang)) =
  "{\"error\":\"Unsupported script language: " <> toBS (show lang) <> "\"}"
renderPipelineError (PipelineProxyError (ProxyConnectionError msg)) =
  "{\"error\":\"Proxy connection error: " <> encodeUtf8 msg <> "\"}"
renderPipelineError (PipelineProxyError (ProxyHttpError msg)) =
  "{\"error\":\"Proxy HTTP error: " <> encodeUtf8 msg <> "\"}"

toBS :: String -> BS.ByteString
toBS = encodeUtf8 . pack

-- | Collapse consecutive slashes in a path (e.g. "//jira//issues" -> "/jira/issues")
normalizePath :: Text -> Text
normalizePath t =
  let collapsed = T.intercalate "/" (filter (not . T.null) (T.splitOn "/" t))
   in if T.null collapsed then "/" else "/" <> collapsed
