module Middleman.ProxySpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text, pack)
import Middleman.Proxy (ProxyError (..), forwardRequest)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ServiceConfig (..)
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
  ( hContentType
  , methodGet
  , methodPost
  , ok200
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

-- | Simple echo app that returns request info as response body
echoApp :: Wai.Application
echoApp waiReq respond = do
  body <- Wai.consumeRequestBodyStrict waiReq
  let path = Wai.rawPathInfo waiReq
      method = Wai.requestMethod waiReq
      authHeader = lookup "Authorization" (Wai.requestHeaders waiReq)
      responseBody = LBS.fromStrict $
        "method=" <> method
        <> ",path=" <> path
        <> ",auth=" <> maybe "none" id authHeader
        <> ",body=" <> LBS.toStrict body
  respond $ Wai.responseLBS ok200 [(hContentType, "text/plain")] responseBody

spec :: Spec
spec = do
  describe "forwardRequest" $ do
    it "forwards a GET request" $ do
      Warp.testWithApplication (pure echoApp) $ \port -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        let svc = mkSvc port
            route = RouteConfig "/test" "/api/hello" methodGet (ScriptChain [] [])
            req = MiddlemanRequest methodGet "/test" [] "" ""
        result <- forwardRequest manager svc route req
        case result of
          Left err -> expectationFailure ("Proxy error: " <> show err)
          Right resp -> do
            mresStatus resp `shouldBe` ok200
            BS.isInfixOf "method=GET" (mresBody resp) `shouldBe` True
            BS.isInfixOf "path=/api/hello" (mresBody resp) `shouldBe` True

    it "forwards a POST request with body" $ do
      Warp.testWithApplication (pure echoApp) $ \port -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        let svc = mkSvc port
            route = RouteConfig "/test" "/api/data" methodPost (ScriptChain [] [])
            req = MiddlemanRequest methodPost "/test" [] "hello body" ""
        result <- forwardRequest manager svc route req
        case result of
          Left err -> expectationFailure ("Proxy error: " <> show err)
          Right resp -> do
            BS.isInfixOf "method=POST" (mresBody resp) `shouldBe` True
            BS.isInfixOf "body=hello body" (mresBody resp) `shouldBe` True

    it "injects bearer auth header" $ do
      Warp.testWithApplication (pure echoApp) $ \port -> do
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        let svc = mkSvcAuth port Bearer "my-token"
            route = RouteConfig "/test" "/api/test" methodGet (ScriptChain [] [])
            req = MiddlemanRequest methodGet "/test" [] "" ""
        result <- forwardRequest manager svc route req
        case result of
          Left err -> expectationFailure ("Proxy error: " <> show err)
          Right resp -> do
            BS.isInfixOf "auth=Bearer my-token" (mresBody resp) `shouldBe` True

    it "returns error for unreachable target" $ do
      manager <- HTTP.newManager HTTP.defaultManagerSettings
      let svc = ServiceConfig "test" "http://localhost:1" (AuthConfig Bearer "tok" Nothing) [] (ScriptChain [] [])
          route = RouteConfig "/test" "/api/test" methodGet (ScriptChain [] [])
          req = MiddlemanRequest methodGet "/test" [] "" ""
      result <- forwardRequest manager svc route req
      case result of
        Left (ProxyConnectionError _) -> pure ()
        Left err -> expectationFailure ("Wrong error type: " <> show err)
        Right _ -> expectationFailure "Expected connection error"

-- Helpers

mkSvc :: Int -> ServiceConfig
mkSvc port = mkSvcAuth port Bearer "test-token"

mkSvcAuth :: Int -> AuthType -> Text -> ServiceConfig
mkSvcAuth port authTy token =
  ServiceConfig
    { serviceName = "test"
    , serviceBaseUrl = pack ("http://localhost:" <> show port)
    , serviceAuth = AuthConfig authTy token Nothing
    , serviceRoutes = []
    , serviceScripts = ScriptChain [] []
    }
