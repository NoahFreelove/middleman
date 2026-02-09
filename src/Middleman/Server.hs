module Middleman.Server
  ( startServer
  , makeApp
  ) where

import Control.Exception (finally)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack)
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
import Middleman.Proxy (ProxyError (..))
import Middleman.Router (MatchResult (..), matchRoute)
import Middleman.Script (ScriptError (..))
import Middleman.Types
  ( GlobalConfig (..)
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)
  )
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS
import Network.HTTP.Types
  ( forbidden403
  , hContentType
  , internalServerError500
  , methodNotAllowed405
  , notFound404
  , statusCode
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
  let path = decodeUtf8 (Wai.rawPathInfo waiReq)
      method = Wai.requestMethod waiReq
  logRequest logger method path
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
      result <- runPipeline manager cfg svc route params mReq
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
