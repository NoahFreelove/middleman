module Middleman.TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Middleman.Types
  ( AuthConfig (..)
  , AuthType (..)
  , GlobalConfig (..)
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)
  , RouteConfig (..)
  , ScriptChain (..)
  , ScriptLanguage (..)
  , ScriptRef (..)
  , ServiceConfig (..)
  , emptyScriptChain
  )
import Network.HTTP.Types (methodGet, methodPost, ok200, status404)
import Test.Hspec

spec :: Spec
spec = do
  describe "emptyScriptChain" $ do
    it "has no input scripts" $ do
      inputScripts emptyScriptChain `shouldBe` []

    it "has no output scripts" $ do
      outputScripts emptyScriptChain `shouldBe` []

  describe "ScriptRef" $ do
    it "can be constructed with Haskell language" $ do
      let ref = ScriptRef "scripts/test.hs" Haskell
      scriptPath ref `shouldBe` "scripts/test.hs"
      scriptLanguage ref `shouldBe` Haskell

    it "can be constructed with Python language" $ do
      let ref = ScriptRef "scripts/test.py" Python
      scriptLanguage ref `shouldBe` Python

  describe "ScriptChain" $ do
    it "holds ordered lists of scripts" $ do
      let s1 = ScriptRef "a.hs" Haskell
          s2 = ScriptRef "b.hs" Haskell
          chain = ScriptChain [s1] [s2]
      inputScripts chain `shouldBe` [s1]
      outputScripts chain `shouldBe` [s2]

  describe "RouteConfig" $ do
    it "can be constructed" $ do
      let route = RouteConfig "/jira/issues" "/rest/api/3/search" methodGet emptyScriptChain
      routePath route `shouldBe` "/jira/issues"
      routeTargetPath route `shouldBe` "/rest/api/3/search"
      routeMethod route `shouldBe` methodGet

  describe "ServiceConfig" $ do
    it "can be constructed with auth and routes" $ do
      let auth = AuthConfig Bearer "secret" Nothing
          route = RouteConfig "/test" "/api/test" methodGet emptyScriptChain
          svc = ServiceConfig "test-service" "https://example.com" auth [route] emptyScriptChain
      serviceName svc `shouldBe` "test-service"
      serviceBaseUrl svc `shouldBe` "https://example.com"
      length (serviceRoutes svc) `shouldBe` 1

  describe "GlobalConfig" $ do
    it "can be constructed with default port" $ do
      let cfg = GlobalConfig 8080 emptyScriptChain []
      globalPort cfg `shouldBe` 8080
      globalServices cfg `shouldBe` []

  describe "MiddlemanRequest JSON" $ do
    it "round-trips through JSON" $ do
      let req = MiddlemanRequest methodGet "/test" [("Content-Type", "application/json")] "body" "q=1"
      decode (encode req) `shouldBe` Just req

    it "round-trips POST request with empty body" $ do
      let req = MiddlemanRequest methodPost "/api" [] "" ""
      decode (encode req) `shouldBe` Just req

    it "round-trips request with multiple headers" $ do
      let req = MiddlemanRequest methodGet "/x" [("Accept", "text/html"), ("X-Custom", "val")] "" ""
      decode (encode req) `shouldBe` Just req

  describe "MiddlemanResponse JSON" $ do
    it "round-trips through JSON" $ do
      let resp = MiddlemanResponse ok200 [("Content-Type", "text/plain")] "hello"
      decode (encode resp) `shouldBe` Just resp

    it "round-trips 404 status" $ do
      let resp = MiddlemanResponse status404 [] "not found"
      decode (encode resp) `shouldBe` Just resp

    it "round-trips response with empty body" $ do
      let resp = MiddlemanResponse ok200 [] ""
      decode (encode resp) `shouldBe` Just resp
