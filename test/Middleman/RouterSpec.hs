module Middleman.RouterSpec (spec) where

import Data.Text (Text)
import Middleman.Router (MatchResult (..), findOwningService, matchPattern, matchRoute, synthesizeBlanketRoute)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ServiceConfig (..)
  , emptyScriptChain
  )
import Network.HTTP.Types (Method, methodDelete, methodGet, methodPost)
import Test.Hspec

spec :: Spec
spec = do
  describe "matchRoute" $ do
    it "matches an exact route path and method" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/svc1/test" "/api/test" methodGet]]
      case matchRoute cfg "/svc1/test" methodGet of
        RouteMatched svc route _ -> do
          serviceName svc `shouldBe` "svc1"
          routePath route `shouldBe` "/svc1/test"
          routeTargetPath route `shouldBe` "/api/test"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "returns NoRouteFound for unknown path" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/svc1/test" "/api/test" methodGet]]
      case matchRoute cfg "/unknown/path" methodGet of
        NoRouteFound -> pure ()
        other -> expectationFailure ("Expected NoRouteFound, got: " <> show other)

    it "returns MethodNotAllowed for wrong method on explicit route" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/svc1/test" "/api/test" methodGet]]
      case matchRoute cfg "/svc1/test" methodPost of
        MethodNotAllowed p -> p `shouldBe` "/svc1/test"
        other -> expectationFailure ("Expected MethodNotAllowed, got: " <> show other)

    it "returns NoRouteFound for empty config" $ do
      let cfg = mkCfg []
      case matchRoute cfg "/test" methodGet of
        NoRouteFound -> pure ()
        other -> expectationFailure ("Expected NoRouteFound, got: " <> show other)

    it "matches routes across multiple services" $ do
      let cfg = mkCfg
            [ mkSvc "svc1" [mkRoute "/svc1/a" "/api/a1" methodGet]
            , mkSvc "svc2" [mkRoute "/svc2/b" "/api/b" methodGet]
            ]
      case matchRoute cfg "/svc2/b" methodGet of
        RouteMatched svc route _ -> do
          serviceName svc `shouldBe` "svc2"
          routeTargetPath route `shouldBe` "/api/b"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "matches POST route correctly" $ do
      let cfg = mkCfg [mkSvc "svc1"
            [ mkRoute "/svc1/items" "/api/items" methodGet
            , mkRoute "/svc1/items" "/api/items" methodPost
            ]]
      case matchRoute cfg "/svc1/items" methodPost of
        RouteMatched _ route _ -> routeMethod route `shouldBe` methodPost
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "captures path parameters from parameterized route" $ do
      let cfg = mkCfg [mkSvc "svc1" [mkRoute "/svc1/issues/{id}" "/api/issues/{id}" methodGet]]
      case matchRoute cfg "/svc1/issues/PROJ-42" methodGet of
        RouteMatched _ _ params -> params `shouldBe` [("id", "PROJ-42")]
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

  describe "blanket allowedMethods" $ do
    it "blanket GET matches any path under the service" $ do
      let svc = mkSvcBlanket "api" [] [methodGet]
          cfg = mkCfg [svc]
      case matchRoute cfg "/api/anything/here" methodGet of
        RouteMatched s _ _ -> serviceName s `shouldBe` "api"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "explicit route takes priority over blanket for same path/method" $ do
      let svc = mkSvcBlanket "api" [mkRoute "/api/search" "/custom/search" methodGet] [methodGet]
          cfg = mkCfg [svc]
      case matchRoute cfg "/api/search" methodGet of
        RouteMatched _ route _ -> routeTargetPath route `shouldBe` "/custom/search"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "blanket does not match method outside allowedMethods" $ do
      let svc = mkSvcBlanket "api" [] [methodGet]
          cfg = mkCfg [svc]
      case matchRoute cfg "/api/foo" methodPost of
        MethodNotAllowed _ -> pure ()
        other -> expectationFailure ("Expected MethodNotAllowed, got: " <> show other)

    it "synthesized route has correct targetPath (prefix stripped)" $ do
      let svc = mkSvcBlanket "jira" [] [methodGet]
          cfg = mkCfg [svc]
      case matchRoute cfg "/jira/rest/api/3/search" methodGet of
        RouteMatched _ route _ -> routeTargetPath route `shouldBe` "/rest/api/3/search"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "service with no allowedMethods and invert=False behaves like before" $ do
      let svc = mkSvc "svc1" [mkRoute "/svc1/test" "/api/test" methodGet]
          cfg = mkCfg [svc]
      case matchRoute cfg "/svc1/other" methodGet of
        NoRouteFound -> pure ()
        other -> expectationFailure ("Expected NoRouteFound, got: " <> show other)

    it "wrong method on explicit route with blanket covering it still uses blanket" $ do
      let svc = mkSvcBlanket "api" [mkRoute "/api/items" "/custom/items" methodGet] [methodGet, methodPost]
          cfg = mkCfg [svc]
      case matchRoute cfg "/api/items" methodPost of
        RouteMatched _ route _ -> do
          -- Blanket takes over since explicit route is GET-only
          routeTargetPath route `shouldBe` "/items"
          routeMethod route `shouldBe` methodPost
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

  describe "inverted (denylist) mode" $ do
    it "denies explicitly listed route+method" $ do
      let svc = mkSvcInverted "github"
                  [mkRoute "/github/admin/settings" "/admin/settings" methodGet]
                  [methodGet, methodPost]
          cfg = mkCfg [svc]
      case matchRoute cfg "/github/admin/settings" methodGet of
        RouteDenied _ -> pure ()
        other -> expectationFailure ("Expected RouteDenied, got: " <> show other)

    it "allows non-listed path" $ do
      let svc = mkSvcInverted "github"
                  [mkRoute "/github/admin/settings" "/admin/settings" methodGet]
                  [methodGet, methodPost]
          cfg = mkCfg [svc]
      case matchRoute cfg "/github/repos" methodGet of
        RouteMatched s _ _ -> serviceName s `shouldBe` "github"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "denies only the matching method (deny GET, POST still allowed)" $ do
      let svc = mkSvcInverted "github"
                  [mkRoute "/github/admin/settings" "/admin/settings" methodGet]
                  [methodGet, methodPost]
          cfg = mkCfg [svc]
      case matchRoute cfg "/github/admin/settings" methodPost of
        RouteMatched s _ _ -> serviceName s `shouldBe` "github"
        other -> expectationFailure ("Expected RouteMatched, got: " <> show other)

    it "returns MethodNotAllowed for method not in allowedMethods" $ do
      let svc = mkSvcInverted "github"
                  []
                  [methodGet, methodPost]
          cfg = mkCfg [svc]
      case matchRoute cfg "/github/repos" methodDelete of
        MethodNotAllowed _ -> pure ()
        other -> expectationFailure ("Expected MethodNotAllowed, got: " <> show other)

  describe "findOwningService" $ do
    it "returns correct service for matching prefix" $ do
      let svcs = [mkSvc "jira" [], mkSvc "github" []]
      case findOwningService "/jira/issues" svcs of
        Just svc -> serviceName svc `shouldBe` "jira"
        Nothing -> expectationFailure "Expected Just"

    it "returns Nothing for unknown prefix" $ do
      let svcs = [mkSvc "jira" []]
      findOwningService "/unknown/path" svcs `shouldBe` Nothing

    it "does not false-match service prefix substring" $ do
      let svcs = [mkSvc "api" []]
      findOwningService "/api-v2/foo" svcs `shouldBe` Nothing

  describe "synthesizeBlanketRoute" $ do
    it "strips service prefix from path" $ do
      let route = synthesizeBlanketRoute "jira" "/jira/rest/api/search" methodGet
      routeTargetPath route `shouldBe` "/rest/api/search"

    it "returns / for service root" $ do
      let route = synthesizeBlanketRoute "jira" "/jira" methodGet
      routeTargetPath route `shouldBe` "/"

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
  ServiceConfig name "https://example.com" (AuthConfig Bearer "tok" Nothing) routes emptyScriptChain [] False

mkSvcBlanket :: Text -> [RouteConfig] -> [Method] -> ServiceConfig
mkSvcBlanket name routes methods =
  ServiceConfig name "https://example.com" (AuthConfig Bearer "tok" Nothing) routes emptyScriptChain methods False

mkSvcInverted :: Text -> [RouteConfig] -> [Method] -> ServiceConfig
mkSvcInverted name routes methods =
  ServiceConfig name "https://example.com" (AuthConfig Bearer "tok" Nothing) routes emptyScriptChain methods True

mkRoute :: Text -> Text -> Method -> RouteConfig
mkRoute path target method = RouteConfig path target method (ScriptChain [] [])
