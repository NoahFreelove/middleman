module Middleman.ScriptSpec (spec) where

import Middleman.Script (ScriptError (..), runInputScript, runOutputScript)
import Middleman.Types
  ( MiddlemanRequest (..)
  , MiddlemanResponse (..)
  , ScriptLanguage (..)
  , ScriptRef (..)
  )
import Network.HTTP.Types (methodGet, ok200)
import Test.Hspec

spec :: Spec
spec = do
  describe "runInputScript (Haskell)" $ do
    it "executes Haskell identity input script via hint" $ do
      let ref = ScriptRef "scripts/example/identity.hs" Haskell
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> r `shouldBe` req

    it "executes Haskell script that modifies request" $ do
      let ref = ScriptRef "scripts/test/modify-request.hs" Haskell
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> mrHeaders r `shouldBe` [("X-Modified-By", "haskell-script")]

    it "returns ScriptLoadError for missing Haskell script" $ do
      let ref = ScriptRef "scripts/nonexistent.hs" Haskell
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left (ScriptLoadError _) -> pure ()
        other -> expectationFailure ("Expected ScriptLoadError, got: " <> show other)

  describe "runOutputScript (Haskell)" $ do
    it "executes Haskell identity output script via hint" $ do
      let ref = ScriptRef "scripts/example/identity-output.hs" Haskell
          resp = MiddlemanResponse ok200 [] "body"
      result <- runOutputScript ref resp
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> r `shouldBe` resp

  describe "runInputScript (Python)" $ do
    it "executes Python identity input script" $ do
      let ref = ScriptRef "scripts/example/identity.py" Python
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> r `shouldBe` req

    it "executes Python script that modifies request" $ do
      let ref = ScriptRef "scripts/test/modify-request.py" Python
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> mrHeaders r `shouldBe` [("X-Modified-By", "python-script")]

    it "returns ScriptLoadError for missing Python script" $ do
      let ref = ScriptRef "scripts/nonexistent.py" Python
          req = MiddlemanRequest methodGet "/test" [] "" "" "" ""
      result <- runInputScript ref req
      case result of
        Left (ScriptLoadError _) -> pure ()
        other -> expectationFailure ("Expected ScriptLoadError, got: " <> show other)

  describe "runOutputScript (Python)" $ do
    it "executes Python identity output script" $ do
      let ref = ScriptRef "scripts/example/identity.py" Python
          resp = MiddlemanResponse ok200 [] "body"
      result <- runOutputScript ref resp
      case result of
        Left err -> expectationFailure ("Script error: " <> show err)
        Right r -> r `shouldBe` resp
