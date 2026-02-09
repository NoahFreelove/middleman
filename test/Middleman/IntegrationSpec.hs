module Middleman.IntegrationSpec (spec) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (pack)
import Middleman.Config (normalizeConfig)
import Middleman.Logging (newLogger)
import Middleman.Server (makeApp)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , RouteConfig (..)
  , ServiceConfig (..)
  , emptyScriptChain
  )
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types
  ( hContentType
  , methodGet
  , methodNotAllowed405
  , methodPost
  , notFound404
  , ok200
  )
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Test.Hspec

-- | Echo app that returns request details
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
  describe "end-to-end proxy" $ do
    it "proxies a GET request through the full pipeline" $ do
      -- Start mock target server
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        -- Build middleman config pointing at mock target
        let cfg = mkConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        -- Start middleman as a WAI app
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          -- Send request to middleman
          req <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/test/get")
          resp <- HTTP.httpLbs req manager
          HTTP.responseStatus resp `shouldBe` ok200
          let body = LBS.toStrict (HTTP.responseBody resp)
          BS.isInfixOf "method=GET" body `shouldBe` True
          BS.isInfixOf "path=/api/get" body `shouldBe` True
          BS.isInfixOf "auth=Bearer test-secret" body `shouldBe` True

    it "proxies a POST request with body" $ do
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        let cfg = mkConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          initReq <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/test/post")
          let req = initReq
                { HTTP.method = methodPost
                , HTTP.requestBody = HTTP.RequestBodyBS "test body"
                }
          resp <- HTTP.httpLbs req manager
          HTTP.responseStatus resp `shouldBe` ok200
          let body = LBS.toStrict (HTTP.responseBody resp)
          BS.isInfixOf "method=POST" body `shouldBe` True
          BS.isInfixOf "body=test body" body `shouldBe` True

    it "returns 404 for unknown routes" $ do
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        let cfg = mkConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          initReq <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/unknown")
          let req = initReq { HTTP.checkResponse = \_ _ -> pure () }
          resp <- HTTP.httpLbs req manager
          HTTP.responseStatus resp `shouldBe` notFound404

    it "returns 405 for wrong method" $ do
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        let cfg = mkConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          initReq <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/test/get")
          let req = initReq
                { HTTP.method = methodPost
                , HTTP.checkResponse = \_ _ -> pure ()
                }
          resp <- HTTP.httpLbs req manager
          HTTP.responseStatus resp `shouldBe` methodNotAllowed405

    it "injects auth header from config" $ do
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        let cfg = mkConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          req <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/test/get")
          resp <- HTTP.httpLbs req manager
          let body = LBS.toStrict (HTTP.responseBody resp)
          BS.isInfixOf "auth=Bearer test-secret" body `shouldBe` True

    it "forwards parameterized route with substitution" $ do
      Warp.testWithApplication (pure echoApp) $ \targetPort -> do
        let cfg = mkParamConfig targetPort
        logger <- newLogger
        manager <- HTTP.newManager HTTP.defaultManagerSettings
        Warp.testWithApplication (pure (makeApp logger manager cfg)) $ \mmPort -> do
          req <- HTTP.parseRequest ("http://localhost:" <> show mmPort <> "/test/items/PROJ-42")
          resp <- HTTP.httpLbs req manager
          HTTP.responseStatus resp `shouldBe` ok200
          let body = LBS.toStrict (HTTP.responseBody resp)
          BS.isInfixOf "path=/api/items/PROJ-42" body `shouldBe` True

-- Helpers

mkConfig :: Int -> GlobalConfig
mkConfig targetPort =
  normalizeConfig
    GlobalConfig
      { globalPort = 0  -- not used directly in testWithApplication
      , globalScripts = emptyScriptChain
      , globalServices =
          [ ServiceConfig
              { serviceName = "test"
              , serviceBaseUrl = pack ("http://localhost:" <> show targetPort)
              , serviceAuth = AuthConfig Bearer "test-secret" Nothing
              , serviceRoutes =
                  [ RouteConfig "/get" "/api/get" methodGet emptyScriptChain
                  , RouteConfig "/post" "/api/post" methodPost emptyScriptChain
                  ]
              , serviceScripts = emptyScriptChain
              }
          ]
      }

mkParamConfig :: Int -> GlobalConfig
mkParamConfig targetPort =
  normalizeConfig
    GlobalConfig
      { globalPort = 0
      , globalScripts = emptyScriptChain
      , globalServices =
          [ ServiceConfig
              { serviceName = "test"
              , serviceBaseUrl = pack ("http://localhost:" <> show targetPort)
              , serviceAuth = AuthConfig Bearer "test-secret" Nothing
              , serviceRoutes =
                  [ RouteConfig "/items/{id}" "/api/items/{id}" methodGet emptyScriptChain
                  ]
              , serviceScripts = emptyScriptChain
              }
          ]
      }
