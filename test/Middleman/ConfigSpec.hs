module Middleman.ConfigSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import Middleman.Config
  ( ConfigError (..)
  , normalizeConfig
  , parseConfig
  , renderConfigError
  , validateConfig
  )
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ScriptLanguage (..)
  , ScriptRef (..)
  , ServiceConfig (..)
  )
import Network.HTTP.Types (methodGet, methodPost)
import Test.Hspec

validConfigJson :: LBS.ByteString
validConfigJson =
  "{\"port\":9090,\"services\":[{\"name\":\"test\",\"baseUrl\":\"https://example.com\",\
  \\"auth\":{\"type\":\"bearer\",\"token\":\"secret\"},\"routes\":[{\"path\":\"/hello\",\
  \\"targetPath\":\"/api/hello\",\"method\":\"GET\"}]}]}"

minimalConfigJson :: LBS.ByteString
minimalConfigJson = "{}"

spec :: Spec
spec = do
  describe "parseConfig" $ do
    it "parses a valid config" $ do
      case parseConfig validConfigJson of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          globalPort cfg `shouldBe` 9090
          length (globalServices cfg) `shouldBe` 1

    it "uses default port when not specified" $ do
      case parseConfig minimalConfigJson of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          globalPort cfg `shouldBe` 8080
          globalServices cfg `shouldBe` []

    it "parses service auth correctly" $ do
      case parseConfig validConfigJson of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let svc = Prelude.head (globalServices cfg)
          serviceName svc `shouldBe` "test"
          serviceBaseUrl svc `shouldBe` "https://example.com"
          authType (serviceAuth svc) `shouldBe` Bearer
          authToken (serviceAuth svc) `shouldBe` "secret"

    it "parses route method correctly" $ do
      case parseConfig validConfigJson of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let route = Prelude.head (serviceRoutes (Prelude.head (globalServices cfg)))
          routePath route `shouldBe` "/hello"
          routeTargetPath route `shouldBe` "/api/hello"
          routeMethod route `shouldBe` methodGet

    it "defaults method to GET" $ do
      let json = "{\"services\":[{\"name\":\"s\",\"baseUrl\":\"https://x.com\",\
                 \\"auth\":{\"type\":\"bearer\",\"token\":\"t\"},\"routes\":[{\"path\":\"/p\",\
                 \\"targetPath\":\"/t\"}]}]}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let route = Prelude.head (serviceRoutes (Prelude.head (globalServices cfg)))
          routeMethod route `shouldBe` methodGet

    it "parses POST method" $ do
      let json = "{\"services\":[{\"name\":\"s\",\"baseUrl\":\"https://x.com\",\
                 \\"auth\":{\"type\":\"bearer\",\"token\":\"t\"},\"routes\":[{\"path\":\"/p\",\
                 \\"targetPath\":\"/t\",\"method\":\"POST\"}]}]}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let route = Prelude.head (serviceRoutes (Prelude.head (globalServices cfg)))
          routeMethod route `shouldBe` methodPost

    it "parses basic auth type" $ do
      let json = "{\"services\":[{\"name\":\"s\",\"baseUrl\":\"https://x.com\",\
                 \\"auth\":{\"type\":\"basic\",\"token\":\"user:pass\"},\"routes\":[]}]}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let svc = Prelude.head (globalServices cfg)
          authType (serviceAuth svc) `shouldBe` BasicAuth

    it "parses header auth with custom header name" $ do
      let json = "{\"services\":[{\"name\":\"s\",\"baseUrl\":\"https://x.com\",\
                 \\"auth\":{\"type\":\"header\",\"token\":\"key123\",\"headerName\":\"X-Api-Key\"},\"routes\":[]}]}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let svc = Prelude.head (globalServices cfg)
          authType (serviceAuth svc) `shouldBe` HeaderAuth
          authHeaderName (serviceAuth svc) `shouldBe` Just "X-Api-Key"

    it "infers Haskell language from .hs extension" $ do
      let json = "{\"globalScripts\":{\"input\":[\"scripts/test.hs\"],\"output\":[]}}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let scripts = inputScripts (globalScripts cfg)
          length scripts `shouldBe` 1
          scriptLanguage (Prelude.head scripts) `shouldBe` Haskell
          scriptPath (Prelude.head scripts) `shouldBe` "scripts/test.hs"

    it "infers Python language from .py extension" $ do
      let json = "{\"globalScripts\":{\"input\":[\"scripts/test.py\"],\"output\":[]}}"
      case parseConfig json of
        Left err -> expectationFailure ("Parse failed: " <> show err)
        Right cfg -> do
          let scripts = inputScripts (globalScripts cfg)
          scriptLanguage (Prelude.head scripts) `shouldBe` Python

    it "returns error on invalid JSON" $ do
      case parseConfig "not json" of
        Left (ConfigParseError _) -> pure ()
        Left err -> expectationFailure ("Wrong error type: " <> show err)
        Right _ -> expectationFailure "Expected parse error"

    it "returns error on unknown auth type" $ do
      let json = "{\"services\":[{\"name\":\"s\",\"baseUrl\":\"https://x.com\",\
                 \\"auth\":{\"type\":\"unknown\",\"token\":\"t\"},\"routes\":[]}]}"
      case parseConfig json of
        Left (ConfigParseError _) -> pure ()
        Left err -> expectationFailure ("Wrong error type: " <> show err)
        Right _ -> expectationFailure "Expected parse error"

  describe "validateConfig" $ do
    it "rejects port 0" $ do
      let cfg = mkConfig 0 []
      validateConfig cfg `shouldBe` Left (ConfigValidationError "Invalid port: 0. Must be between 1 and 65535.")

    it "rejects port 70000" $ do
      let cfg = mkConfig 70000 []
      validateConfig cfg `shouldBe` Left (ConfigValidationError "Invalid port: 70000. Must be between 1 and 65535.")

    it "accepts port 8080" $ do
      let cfg = mkConfig 8080 []
      validateConfig cfg `shouldBe` Right cfg

    it "rejects duplicate route paths" $ do
      let svc1 = mkService "shared" [mkRoute "/dup" "/a"]
          svc2 = mkService "shared" [mkRoute "/dup" "/b"]
          cfg = mkConfig 8080 [svc1, svc2]
      validateConfig (normalizeConfig cfg) `shouldBe` Left (ConfigValidationError "Duplicate route paths found across services.")

    it "rejects empty baseUrl" $ do
      let svc = mkServiceWithUrl "svc" "" [mkRoute "/test" "/t"]
          cfg = mkConfig 8080 [svc]
      validateConfig cfg `shouldBe` Left (ConfigValidationError "Service 'svc' has empty baseUrl.")

  describe "normalizeConfig" $ do
    it "prefixes route paths with service name" $ do
      let svc = mkService "jira" [mkRoute "/issues" "/api/issues"]
          cfg = mkConfig 8080 [svc]
          normalized = normalizeConfig cfg
          routes = serviceRoutes (Prelude.head (globalServices normalized))
      routePath (Prelude.head routes) `shouldBe` "/jira/issues"

    it "does not modify targetPath" $ do
      let svc = mkService "jira" [mkRoute "/issues" "/api/issues"]
          cfg = mkConfig 8080 [svc]
          normalized = normalizeConfig cfg
          routes = serviceRoutes (Prelude.head (globalServices normalized))
      routeTargetPath (Prelude.head routes) `shouldBe` "/api/issues"

    it "handles route path without leading slash" $ do
      let svc = mkService "jira" [mkRoute "issues" "/api/issues"]
          cfg = mkConfig 8080 [svc]
          normalized = normalizeConfig cfg
          routes = serviceRoutes (Prelude.head (globalServices normalized))
      routePath (Prelude.head routes) `shouldBe` "/jira/issues"

    it "handles route path with leading slash" $ do
      let svc = mkService "jira" [mkRoute "/issues/{id}" "/api/issues/{id}"]
          cfg = mkConfig 8080 [svc]
          normalized = normalizeConfig cfg
          routes = serviceRoutes (Prelude.head (globalServices normalized))
      routePath (Prelude.head routes) `shouldBe` "/jira/issues/{id}"

  describe "renderConfigError" $ do
    it "renders ConfigFileNotFound" $ do
      renderConfigError (ConfigFileNotFound "/missing.json")
        `shouldBe` "Config file not found: /missing.json"

    it "renders ConfigParseError" $ do
      renderConfigError (ConfigParseError "bad json")
        `shouldBe` "Config parse error: bad json"

    it "renders ConfigValidationError" $ do
      renderConfigError (ConfigValidationError "bad port")
        `shouldBe` "Config validation error: bad port"

-- Helpers

mkConfig :: Int -> [ServiceConfig] -> GlobalConfig
mkConfig port svcs = GlobalConfig port emptyChain svcs
  where
    emptyChain = ScriptChain [] []

mkService :: Text -> [RouteConfig] -> ServiceConfig
mkService name = mkServiceWithUrl name "https://example.com"

mkServiceWithUrl :: Text -> Text -> [RouteConfig] -> ServiceConfig
mkServiceWithUrl name url routes =
  ServiceConfig name url (AuthConfig Bearer "tok" Nothing) routes (ScriptChain [] [])

mkRoute :: Text -> Text -> RouteConfig
mkRoute path target = RouteConfig path target methodGet (ScriptChain [] [])
