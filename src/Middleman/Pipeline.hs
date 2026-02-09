module Middleman.Pipeline
  ( PipelineError (..)
  , buildInputChain
  , buildOutputChain
  , runPipeline
  ) where

import qualified Network.HTTP.Client as HTTP
import Middleman.Proxy (ProxyError, forwardRequest)
import Middleman.Script (ScriptError, runInputScript, runOutputScript)
import Middleman.Types
  ( GlobalConfig (..)
  , MiddlemanRequest
  , MiddlemanResponse
  , RouteConfig (..)
  , ScriptChain (..)
  , ScriptRef
  , ServiceConfig (..)
  )

-- | Errors that can occur in the pipeline
data PipelineError
  = PipelineScriptError ScriptError
  | PipelineProxyError ProxyError
  deriving (Show, Eq)

-- | Build the ordered input script chain: global -> service -> route
buildInputChain :: GlobalConfig -> ServiceConfig -> RouteConfig -> [ScriptRef]
buildInputChain cfg svc route =
  inputScripts (globalScripts cfg)
    <> inputScripts (serviceScripts svc)
    <> inputScripts (routeScripts route)

-- | Build the ordered output script chain: route -> service -> global
buildOutputChain :: GlobalConfig -> ServiceConfig -> RouteConfig -> [ScriptRef]
buildOutputChain cfg svc route =
  outputScripts (routeScripts route)
    <> outputScripts (serviceScripts svc)
    <> outputScripts (globalScripts cfg)

-- | Run the full pipeline: input scripts -> forward -> output scripts
runPipeline
  :: HTTP.Manager
  -> GlobalConfig
  -> ServiceConfig
  -> RouteConfig
  -> MiddlemanRequest
  -> IO (Either PipelineError MiddlemanResponse)
runPipeline manager cfg svc route req = do
  let inChain = buildInputChain cfg svc route
      outChain = buildOutputChain cfg svc route
  -- Run input scripts (foldM)
  inputResult <- runScriptChain runInputScript inChain req
  case inputResult of
    Left err -> pure (Left (PipelineScriptError err))
    Right transformedReq -> do
      -- Forward to target
      proxyResult <- forwardRequest manager svc route transformedReq
      case proxyResult of
        Left err -> pure (Left (PipelineProxyError err))
        Right resp -> do
          -- Run output scripts
          outputResult <- runScriptChain runOutputScript outChain resp
          case outputResult of
            Left err -> pure (Left (PipelineScriptError err))
            Right transformedResp -> pure (Right transformedResp)

-- | Run a chain of scripts, threading the value through each
runScriptChain
  :: (ScriptRef -> a -> IO (Either ScriptError a))
  -> [ScriptRef]
  -> a
  -> IO (Either ScriptError a)
runScriptChain _ [] val = pure (Right val)
runScriptChain runner (ref : refs) val = do
  result <- runner ref val
  case result of
    Left err -> pure (Left err)
    Right val' -> runScriptChain runner refs val'
