module Middleman.Script.Haskell
  ( runHaskellInputScript
  , runHaskellOutputScript
  ) where

import qualified Data.Text as T
import qualified Language.Haskell.Interpreter as Hint
import Language.Haskell.Interpreter.Unsafe (unsafeRunInterpreterWithArgs)
import Middleman.Script.Types (ScriptError (..))
import Middleman.Types (MiddlemanRequest, MiddlemanResponse)
import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents, getHomeDirectory)

-- | Run a Haskell input script using the hint interpreter.
-- The script must export: transform :: MiddlemanRequest -> IO MiddlemanRequest
runHaskellInputScript :: FilePath -> MiddlemanRequest -> IO (Either ScriptError MiddlemanRequest)
runHaskellInputScript path req = do
  exists <- doesFileExist path
  if not exists
    then pure (Left (ScriptLoadError ("Script file not found: " <> T.pack path)))
    else do
      args <- findPackageDbArgs
      result <- unsafeRunInterpreterWithArgs args $ do
        Hint.set [Hint.searchPath Hint.:= ["src"]]
        Hint.loadModules [path]
        mods <- Hint.getLoadedModules
        Hint.setTopLevelModules mods
        Hint.setImportsQ [("Prelude", Nothing)]
        fn <- Hint.interpret "transform" (Hint.as :: MiddlemanRequest -> IO MiddlemanRequest)
        Hint.liftIO (fn req)
      pure (mapInterpreterError result)

-- | Run a Haskell output script using the hint interpreter.
-- The script must export: transform :: MiddlemanResponse -> IO MiddlemanResponse
runHaskellOutputScript :: FilePath -> MiddlemanResponse -> IO (Either ScriptError MiddlemanResponse)
runHaskellOutputScript path resp = do
  exists <- doesFileExist path
  if not exists
    then pure (Left (ScriptLoadError ("Script file not found: " <> T.pack path)))
    else do
      args <- findPackageDbArgs
      result <- unsafeRunInterpreterWithArgs args $ do
        Hint.set [Hint.searchPath Hint.:= ["src"]]
        Hint.loadModules [path]
        mods <- Hint.getLoadedModules
        Hint.setTopLevelModules mods
        Hint.setImportsQ [("Prelude", Nothing)]
        fn <- Hint.interpret "transform" (Hint.as :: MiddlemanResponse -> IO MiddlemanResponse)
        Hint.liftIO (fn resp)
      pure (mapInterpreterError result)

-- | Discover the cabal store and project package databases so that hint can
-- find both external dependencies (aeson, http-types, etc.) and the middleman
-- library itself.
findPackageDbArgs :: IO [String]
findPackageDbArgs = do
  storeDbs <- findCabalStorePackageDbs
  let projectDb = "dist-newstyle/packagedb/ghc-9.10.3"
  projectExists <- doesDirectoryExist projectDb
  let dbs = storeDbs ++ [projectDb | projectExists]
      dbArgs = ["-package-db=" ++ db | db <- dbs]
      extArgs =
        [ "-XOverloadedStrings"
        , "-XDeriveGeneric"
        , "-XDeriveAnyClass"
        , "-XRecordWildCards"
        , "-XScopedTypeVariables"
        , "-XTypeApplications"
        ]
  pure (dbArgs ++ extArgs)

-- | Find cabal store package databases under ~/.cabal/store
findCabalStorePackageDbs :: IO [FilePath]
findCabalStorePackageDbs = do
  home <- getHomeDirectory
  let storeDir = home ++ "/.cabal/store"
  storeExists <- doesDirectoryExist storeDir
  if not storeExists
    then pure []
    else do
      entries <- getDirectoryContents storeDir
      let ghcDirs = filter (\e -> e /= "." && e /= "..") entries
      dbs <- mapM (\d -> do
        let dbPath = storeDir ++ "/" ++ d ++ "/package.db"
        exists <- doesDirectoryExist dbPath
        pure [dbPath | exists]) ghcDirs
      pure (concat dbs)

-- | Map hint InterpreterError to our ScriptError type.
mapInterpreterError :: Either Hint.InterpreterError a -> Either ScriptError a
mapInterpreterError (Right a) = Right a
mapInterpreterError (Left err) = Left $ case err of
  Hint.WontCompile errs ->
    ScriptLoadError (T.pack (unlines (map Hint.errMsg errs)))
  Hint.NotAllowed msg ->
    ScriptRuntimeError ("Not allowed: " <> T.pack msg)
  Hint.GhcException msg ->
    ScriptRuntimeError ("GHC exception: " <> T.pack msg)
  Hint.UnknownError msg ->
    ScriptRuntimeError ("Unknown error: " <> T.pack msg)
