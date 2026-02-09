module Middleman.PipelineSpec (spec) where

import Middleman.Pipeline (buildInputChain, buildOutputChain)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ScriptLanguage (..)
  , ScriptRef (..)
  , ServiceConfig (..)
  , emptyScriptChain
  )
import Network.HTTP.Types (methodGet)
import Test.Hspec

spec :: Spec
spec = do
  describe "buildInputChain" $ do
    it "returns empty chain when no scripts defined" $ do
      let cfg = mkCfg emptyScriptChain
          svc = mkSvc emptyScriptChain
          route = mkRoute emptyScriptChain
      buildInputChain cfg svc route `shouldBe` []

    it "orders global -> service -> route input scripts" $ do
      let globalScript = ScriptRef "global.hs" Haskell
          svcScript = ScriptRef "svc.hs" Haskell
          routeScript = ScriptRef "route.hs" Haskell
          cfg = mkCfg (ScriptChain [globalScript] [])
          svc = mkSvc (ScriptChain [svcScript] [])
          route = mkRoute (ScriptChain [routeScript] [])
      buildInputChain cfg svc route `shouldBe` [globalScript, svcScript, routeScript]

    it "handles partial chains (only global)" $ do
      let globalScript = ScriptRef "global.hs" Haskell
          cfg = mkCfg (ScriptChain [globalScript] [])
          svc = mkSvc emptyScriptChain
          route = mkRoute emptyScriptChain
      buildInputChain cfg svc route `shouldBe` [globalScript]

    it "handles partial chains (only route)" $ do
      let routeScript = ScriptRef "route.hs" Haskell
          cfg = mkCfg emptyScriptChain
          svc = mkSvc emptyScriptChain
          route = mkRoute (ScriptChain [routeScript] [])
      buildInputChain cfg svc route `shouldBe` [routeScript]

  describe "buildOutputChain" $ do
    it "returns empty chain when no scripts defined" $ do
      let cfg = mkCfg emptyScriptChain
          svc = mkSvc emptyScriptChain
          route = mkRoute emptyScriptChain
      buildOutputChain cfg svc route `shouldBe` []

    it "orders route -> service -> global output scripts" $ do
      let globalScript = ScriptRef "global.hs" Haskell
          svcScript = ScriptRef "svc.hs" Haskell
          routeScript = ScriptRef "route.hs" Haskell
          cfg = mkCfg (ScriptChain [] [globalScript])
          svc = mkSvc (ScriptChain [] [svcScript])
          route = mkRoute (ScriptChain [] [routeScript])
      buildOutputChain cfg svc route `shouldBe` [routeScript, svcScript, globalScript]

    it "handles partial chains (only service)" $ do
      let svcScript = ScriptRef "svc.hs" Haskell
          cfg = mkCfg emptyScriptChain
          svc = mkSvc (ScriptChain [] [svcScript])
          route = mkRoute emptyScriptChain
      buildOutputChain cfg svc route `shouldBe` [svcScript]

-- Helpers

mkCfg :: ScriptChain -> GlobalConfig
mkCfg scripts = GlobalConfig 8080 scripts []

mkSvc :: ScriptChain -> ServiceConfig
mkSvc scripts =
  ServiceConfig "test" "https://example.com" (AuthConfig Bearer "tok" Nothing) [] scripts

mkRoute :: ScriptChain -> RouteConfig
mkRoute = RouteConfig "/test" "/api/test" methodGet
