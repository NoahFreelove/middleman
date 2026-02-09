module Middleman.Types
  ( -- * Script Types
    ScriptLanguage (..)
  , ScriptRef (..)
  , ScriptChain (..)
  , emptyScriptChain

    -- * Auth Types
  , AuthType (..)
  , AuthConfig (..)

    -- * Route and Service Config
  , RouteConfig (..)
  , ServiceConfig (..)
  , GlobalConfig (..)

    -- * Request/Response Types
  , MiddlemanRequest (..)
  , MiddlemanResponse (..)

    -- * Path Parameters
  , PathParams
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Types (Method, Status, mkStatus, statusCode, statusMessage)
import Network.HTTP.Types.Header (Header)

-- | Language a script is written in
data ScriptLanguage
  = Haskell
  | Python
  deriving (Show, Eq, Generic)

-- | Reference to a script file
data ScriptRef = ScriptRef
  { scriptPath :: FilePath
  , scriptLanguage :: ScriptLanguage
  }
  deriving (Show, Eq, Generic)

-- | Ordered lists of input and output scripts
data ScriptChain = ScriptChain
  { inputScripts :: [ScriptRef]
  , outputScripts :: [ScriptRef]
  }
  deriving (Show, Eq, Generic)

-- | Empty script chain with no scripts
emptyScriptChain :: ScriptChain
emptyScriptChain = ScriptChain [] []

-- | Authentication type
data AuthType
  = Bearer
  | BasicAuth
  | HeaderAuth
  deriving (Show, Eq, Generic)

-- | Authentication configuration
data AuthConfig = AuthConfig
  { authType :: AuthType
  , authToken :: Text
  , authHeaderName :: Maybe Text
  -- ^ Custom header name, used only with HeaderAuth
  }
  deriving (Show, Eq, Generic)

-- | A single proxied route within a service
data RouteConfig = RouteConfig
  { routePath :: Text
  -- ^ Local path agents call, e.g., "/jira/issues"
  , routeTargetPath :: Text
  -- ^ Path on the target service
  , routeMethod :: Method
  -- ^ HTTP method
  , routeScripts :: ScriptChain
  -- ^ Scripts specific to this route
  }
  deriving (Show, Eq, Generic)

-- | A target external service (e.g., Jira)
data ServiceConfig = ServiceConfig
  { serviceName :: Text
  , serviceBaseUrl :: Text
  -- ^ e.g., "https://mycompany.atlassian.net"
  , serviceAuth :: AuthConfig
  , serviceRoutes :: [RouteConfig]
  , serviceScripts :: ScriptChain
  -- ^ Scripts that apply to all routes in this service
  }
  deriving (Show, Eq, Generic)

-- | Top-level configuration
data GlobalConfig = GlobalConfig
  { globalPort :: Int
  , globalScripts :: ScriptChain
  , globalServices :: [ServiceConfig]
  }
  deriving (Show, Eq, Generic)

-- | Captured path parameters from parameterized route matching
type PathParams = [(Text, Text)]

-- | Simplified request type that scripts operate on (decoupled from WAI)
data MiddlemanRequest = MiddlemanRequest
  { mrMethod :: Method
  , mrPath :: Text
  , mrHeaders :: [Header]
  , mrBody :: ByteString
  , mrQueryString :: ByteString
  }
  deriving (Show, Eq, Generic)

-- | Simplified response type that scripts operate on
data MiddlemanResponse = MiddlemanResponse
  { mresStatus :: Status
  , mresHeaders :: [Header]
  , mresBody :: ByteString
  }
  deriving (Show, Eq, Generic)

-- JSON helpers for Header (CI ByteString, ByteString)

headerToJSON :: Header -> Value
headerToJSON (name, val) =
  object ["name" .= decodeUtf8 (CI.original name), "value" .= decodeUtf8 val]

parseHeader :: Value -> Parser Header
parseHeader = withObject "Header" $ \o -> do
  name <- o .: "name"
  val <- o .: "value"
  pure (CI.mk (encodeUtf8 (name :: Text)), encodeUtf8 (val :: Text))

-- JSON helpers for Method (ByteString)

methodToJSON :: Method -> Value
methodToJSON = toJSON . decodeUtf8

parseMethod :: Value -> Parser Method
parseMethod = withText "Method" (pure . encodeUtf8)

-- JSON helpers for Status

statusToJSON :: Status -> Value
statusToJSON s = object ["code" .= statusCode s, "message" .= decodeUtf8 (statusMessage s)]

parseStatus :: Value -> Parser Status
parseStatus = withObject "Status" $ \o -> do
  code <- o .: "code"
  msg <- o .: "message"
  pure (mkStatus code (encodeUtf8 (msg :: Text)))

-- ToJSON / FromJSON for MiddlemanRequest

instance ToJSON MiddlemanRequest where
  toJSON MiddlemanRequest{..} = object
    [ "method" .= methodToJSON mrMethod
    , "path" .= mrPath
    , "headers" .= map headerToJSON mrHeaders
    , "body" .= decodeUtf8 mrBody
    , "queryString" .= decodeUtf8 mrQueryString
    ]

instance FromJSON MiddlemanRequest where
  parseJSON = withObject "MiddlemanRequest" $ \o -> do
    method <- o .: "method" >>= parseMethod
    path <- o .: "path"
    headers <- o .: "headers" >>= mapM parseHeader
    body <- encodeUtf8 <$> (o .: "body" :: Parser Text)
    qs <- encodeUtf8 <$> (o .: "queryString" :: Parser Text)
    pure (MiddlemanRequest method path headers body qs)

-- ToJSON / FromJSON for MiddlemanResponse

instance ToJSON MiddlemanResponse where
  toJSON MiddlemanResponse{..} = object
    [ "status" .= statusToJSON mresStatus
    , "headers" .= map headerToJSON mresHeaders
    , "body" .= decodeUtf8 mresBody
    ]

instance FromJSON MiddlemanResponse where
  parseJSON = withObject "MiddlemanResponse" $ \o -> do
    status <- o .: "status" >>= parseStatus
    headers <- o .: "headers" >>= mapM parseHeader
    body <- encodeUtf8 <$> (o .: "body" :: Parser Text)
    pure (MiddlemanResponse status headers body)
