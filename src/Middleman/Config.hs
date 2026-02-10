{-# OPTIONS_GHC -Wno-orphans #-}

module Middleman.Config
  ( loadConfig
  , parseConfig
  , validateConfig
  , normalizeConfig
  , module Middleman.Config.Types
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), (.:), (.:?), (.!=), withObject)
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text as T
import Middleman.Config.Types
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
import Network.HTTP.Types (Method, methodDelete, methodGet, methodPatch, methodPost, methodPut)
import System.Directory (doesFileExist)

-- | Load and parse a config file from disk
loadConfig :: FilePath -> IO (Either ConfigError GlobalConfig)
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure (Left (ConfigFileNotFound path))
    else do
      contents <- LBS.readFile path
      pure (parseConfig contents >>= (validateConfig . normalizeConfig))

-- | Parse a JSON ByteString into a GlobalConfig
parseConfig :: ByteString -> Either ConfigError GlobalConfig
parseConfig bs =
  case Aeson.eitherDecode bs of
    Left err -> Left (ConfigParseError (pack err))
    Right cfg -> Right cfg

-- | Reserved service names that cannot be used
reservedServiceNames :: [Text]
reservedServiceNames = ["index"]

-- | Validate a parsed GlobalConfig
validateConfig :: GlobalConfig -> Either ConfigError GlobalConfig
validateConfig cfg = do
  validatePort (globalPort cfg)
  validateNoReservedServiceNames cfg
  validateNoDuplicateRoutes cfg
  validateServiceUrls cfg
  validateUniqueServiceNames cfg
  validateInvertRequiresAllowedMethods cfg
  Right cfg

validatePort :: Int -> Either ConfigError ()
validatePort port
  | port >= 1 && port <= 65535 = Right ()
  | otherwise = Left (ConfigValidationError ("Invalid port: " <> pack (show port) <> ". Must be between 1 and 65535."))

validateNoReservedServiceNames :: GlobalConfig -> Either ConfigError ()
validateNoReservedServiceNames cfg =
  mapM_ check (globalServices cfg)
  where
    check svc
      | toLower (serviceName svc) `elem` reservedServiceNames =
          Left (ConfigValidationError ("Service name '" <> serviceName svc <> "' is reserved."))
      | otherwise = Right ()

validateNoDuplicateRoutes :: GlobalConfig -> Either ConfigError ()
validateNoDuplicateRoutes cfg =
  let allPaths = concatMap (\svc -> map routePath (serviceRoutes svc)) (globalServices cfg)
   in if nub allPaths == allPaths
        then Right ()
        else Left (ConfigValidationError "Duplicate route paths found across services.")

validateServiceUrls :: GlobalConfig -> Either ConfigError ()
validateServiceUrls cfg =
  mapM_ checkUrl (globalServices cfg)
  where
    checkUrl svc
      | baseUrl == "" = Left (ConfigValidationError ("Service '" <> serviceName svc <> "' has empty baseUrl."))
      | otherwise = Right ()
      where
        baseUrl = serviceBaseUrl svc

validateUniqueServiceNames :: GlobalConfig -> Either ConfigError ()
validateUniqueServiceNames cfg =
  let names = map serviceName (globalServices cfg)
   in if nub names == names
        then Right ()
        else Left (ConfigValidationError "Duplicate service names found.")

validateInvertRequiresAllowedMethods :: GlobalConfig -> Either ConfigError ()
validateInvertRequiresAllowedMethods cfg =
  mapM_ check (globalServices cfg)
  where
    check svc
      | serviceInvert svc && null (allowedMethods svc) =
          Left (ConfigValidationError ("Service '" <> serviceName svc <> "' has invert=true but no allowedMethods."))
      | otherwise = Right ()

-- | Infer script language from file extension
inferLanguage :: FilePath -> ScriptLanguage
inferLanguage path
  | hasSuffix ".py" path = Python
  | otherwise = Haskell
  where
    hasSuffix :: String -> String -> Bool
    hasSuffix suffix str = drop (length str - length suffix) str == suffix

-- | Parse a list of script paths into ScriptRefs
parseScriptRefs :: [FilePath] -> [ScriptRef]
parseScriptRefs = map (\p -> ScriptRef p (inferLanguage p))

-- | Parse a method string to a Method
parseMethod :: Text -> Maybe Method
parseMethod t = case toLower t of
  "get" -> Just methodGet
  "post" -> Just methodPost
  "put" -> Just methodPut
  "delete" -> Just methodDelete
  "patch" -> Just methodPatch
  _ -> Nothing

instance FromJSON GlobalConfig where
  parseJSON = withObject "GlobalConfig" $ \o -> do
    port <- o .:? "port" .!= 8080
    scriptObj <- o .:? "globalScripts" .!= Aeson.Object mempty
    scripts <- parseJSON scriptObj
    services <- o .:? "services" .!= []
    pure (GlobalConfig port scripts services)

instance FromJSON ScriptChain where
  parseJSON = withObject "ScriptChain" $ \o -> do
    inPaths <- o .:? "input" .!= ([] :: [FilePath])
    outPaths <- o .:? "output" .!= ([] :: [FilePath])
    pure (ScriptChain (parseScriptRefs inPaths) (parseScriptRefs outPaths))

instance FromJSON ServiceConfig where
  parseJSON = withObject "ServiceConfig" $ \o -> do
    name <- o .: "name"
    baseUrl <- o .: "baseUrl"
    auth <- parseOptionalAuth o
    routes <- o .:? "routes" .!= []
    scriptObj <- o .:? "scripts" .!= Aeson.Object mempty
    scripts <- parseJSON scriptObj
    methodStrs <- o .:? "allowedMethods" .!= ([] :: [Text])
    methods <- mapM (\m -> case parseMethod m of
        Just method -> pure method
        Nothing     -> fail ("Unknown HTTP method in allowedMethods: " <> unpack m)
      ) methodStrs
    basePath <- o .:? "allowedMethodsBasePath" .!= ("" :: Text)
    invert <- o .:? "invert" .!= False
    pure (ServiceConfig name baseUrl auth routes scripts methods basePath invert)

instance FromJSON AuthConfig where
  parseJSON = withObject "AuthConfig" $ \o -> do
    typeStr <- o .: "type" :: Parser Text
    token <- o .: "token"
    headerName <- o .:? "headerName"
    authTy <- case toLower typeStr of
      "bearer" -> pure Bearer
      "basic" -> pure BasicAuth
      "header" -> pure HeaderAuth
      other -> fail ("Unknown auth type: " <> unpack other)
    pure (AuthConfig authTy token headerName)

-- | Parse the optional "auth" field. Treats missing key and {"type":"none"} as Nothing.
parseOptionalAuth :: Aeson.Object -> Parser (Maybe AuthConfig)
parseOptionalAuth o = do
  mVal <- o .:? "auth"
  case mVal of
    Nothing -> pure Nothing
    Just val -> withObject "auth" checkType val
  where
    checkType obj = do
      typeStr <- obj .:? "type" :: Parser (Maybe Text)
      case fmap toLower typeStr of
        Just "none" -> pure Nothing
        _ -> Just <$> parseJSON (Aeson.Object obj)

instance FromJSON RouteConfig where
  parseJSON = withObject "RouteConfig" $ \o -> do
    path <- o .: "path"
    targetPath <- o .:? "targetPath" .!= path
    methodStr <- o .:? "method" .!= ("GET" :: Text)
    method <- case parseMethod methodStr of
      Just m -> pure m
      Nothing -> fail ("Unknown HTTP method: " <> unpack methodStr)
    scriptObj <- o .:? "scripts" .!= Aeson.Object mempty
    scripts <- parseJSON scriptObj
    pure (RouteConfig path targetPath method scripts)

-- | Normalize config by prepending service name to each route's path
normalizeConfig :: GlobalConfig -> GlobalConfig
normalizeConfig cfg =
  cfg { globalServices = map prefixService (globalServices cfg) }

-- | Prefix all routes in a service with the service name
prefixService :: ServiceConfig -> ServiceConfig
prefixService svc =
  svc { serviceRoutes = map (prefixRoute (serviceName svc)) (serviceRoutes svc) }

-- | Prefix a route path with the service name, e.g. "jira" + "/issues/{id}" -> "/jira/issues/{id}"
prefixRoute :: Text -> RouteConfig -> RouteConfig
prefixRoute name route =
  route { routePath = "/" <> name <> ensureLeadingSlash (routePath route) }
  where
    ensureLeadingSlash t
      | T.isPrefixOf "/" t = t
      | otherwise = "/" <> t
