module Middleman.RouterSpec (spec) where

import Data.Text (Text)
import Middleman.Router (MatchResult (..), matchPattern, matchRoute)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ServiceConfig (..)
  , emptyScriptChain
  )
import Network.HTTP.Types (Method, methodGet, methodPost)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchRoute" $ do
    it "matches an exact route path and method" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/test" "/api/test" methodGet]]
      case matchRoute cfg "/test" methodGet of
        RouteMatched svc route _ -> do
          serviceName svc `shouldBe` "svc1"
          routePath route `shouldBe` "/test"
          routeTargetPath route `shouldBe` "/api/test"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "returns NoRouteFound for unknown path" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/test" "/api/test" methodGet]]
      case matchRoute cfg "/unknown" methodGet of
        NoRouteFound -> pure ()
        other -> expectationFailure ("Expected NoRouteFound, got: " <> show other)

    it "returns MethodNotAllowed for wrong method" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/test" "/api/test" methodGet]]
      case matchRoute cfg "/test" methodPost of
        MethodNotAllowed p -> p `shouldBe` "/test"
        other -> expectationFailure ("Expected MethodNotAllowed, got: " <> show other)

    it "returns NoRouteFound for empty config" $ do
      let cfg = mkCfg []
      case matchRoute cfg "/test" methodGet of
        NoRouteFound -> pure ()
        other -> expectationFailure ("Expected NoRouteFound, got: " <> show other)

    it "matches routes across multiple services (first match wins)" $ do
      let cfg = mkCfg
            [ mkSvc "svc1" [mkRoute "/a" "/api/a1" methodGet]
            , mkSvc "svc2" [mkRoute "/b" "/api/b" methodGet]
            ]
      case matchRoute cfg "/b" methodGet of
        RouteMatched svc route _ -> do
          serviceName svc `shouldBe` "svc2"
          routeTargetPath route `shouldBe` "/api/b"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "first-match-wins when same path in multiple services" $ do
      let cfg = mkCfg
            [ mkSvc "svc1" [mkRoute "/shared" "/api/v1" methodGet]
            , mkSvc "svc2" [mkRoute "/shared" "/api/v2" methodGet]
            ]
      case matchRoute cfg "/shared" methodGet of
        RouteMatched svc route _ -> do
          serviceName svc `shouldBe` "svc1"
          routeTargetPath route `shouldBe` "/api/v1"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "matches POST route correctly" $ do
      let cfg = mkCfg [mkSvc "svc1"
            [ mkRoute "/items" "/api/items" methodGet
            , mkRoute "/items" "/api/items" methodPost
            ]]
      case matchRoute cfg "/items" methodPost of
        RouteMatched _ route _ -> routeMethod route `shouldBe` methodPost
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "captures path parameters from parameterized route" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/issues/{id}" "/api/issues/{id}" methodGet]]
      case matchRoute cfg "/issues/PROJ-42" methodGet of
        RouteMatched _ _ params -> params `shouldBe` [("id", "PROJ-42")]
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

  describe "matchPattern" $ do
    it "matches exact path with no params" $ do
      matchPattern "/foo/bar" "/foo/bar" `shouldBe` Just []

    it "captures a single param" $ do
      matchPattern "/issues/{id}" "/issues/123" `shouldBe` Just [("id", "123")]

    it "captures multiple params" $ do
      matchPattern "/projects/{proj}/issues/{id}" "/projects/ACME/issues/42"
        `shouldBe` Just [("proj", "ACME"), ("id", "42")]

    it "returns Nothing for segment count mismatch" $ do
      matchPattern "/a/b" "/a/b/c" `shouldBe` Nothing

    it "returns Nothing for literal mismatch" $ do
      matchPattern "/foo/bar" "/foo/baz" `shouldBe` Nothing

    it "handles leading slash correctly" $ do
      matchPattern "/test" "/test" `shouldBe` Just []

    it "returns Nothing when path has fewer segments" $ do
      matchPattern "/a/b/c" "/a/b" `shouldBe` Nothing

-- Helpers

mkCfg :: [ServiceConfig] -> GlobalConfig
mkCfg = GlobalConfig 8080 emptyScriptChain

mkSvc :: Text -> [RouteConfig] -> ServiceConfig
mkSvc name routes =
  ServiceConfig name "https://example.com" (AuthConfig Bearer "tok" Nothing) routes emptyScriptChain

mkRoute :: Text -> Text -> Method -> RouteConfig
mkRoute path target method = RouteConfig path target method (ScriptChain [] [])
