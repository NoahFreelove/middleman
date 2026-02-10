module Middleman.Router
  ( MatchResult (..)
  , matchRoute
  , matchPattern
  , findOwningService
  , synthesizeBlanketRoute
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Middleman.Types
  ( GlobalConfig (..)
  , PathParams
  , RouteConfig (..)
  , ScriptChain (..)
  , ServiceConfig (..)
  )
import Network.HTTP.Types (Method)

-- | Result of matching an incoming request to a route
data MatchResult
  = RouteMatched ServiceConfig RouteConfig PathParams
  | RouteDenied Text
  | NoRouteFound
  | MethodNotAllowed Text
  deriving (Show)

-- | Match an incoming request path and method to a route.
-- Dispatches to per-service matching based on path prefix.
matchRoute :: GlobalConfig -> Text -> Method -> MatchResult
matchRoute cfg path method =
  case findOwningService path (globalServices cfg) of
    Nothing  -> NoRouteFound
    Just svc
      | serviceInvert svc -> matchInverted svc path method
      | otherwise         -> matchNormal svc path method

-- | Find which service owns a path by checking if path starts with /<serviceName>/
-- or equals /<serviceName> exactly.
findOwningService :: Text -> [ServiceConfig] -> Maybe ServiceConfig
findOwningService path = go
  where
    go [] = Nothing
    go (svc : rest)
      | T.isPrefixOf prefix path = Just svc
      | path == ("/" <> serviceName svc) = Just svc
      | otherwise = go rest
      where
        prefix = "/" <> serviceName svc <> "/"

-- | Allowlist mode: explicit routes take priority, then blanket allowedMethods.
matchNormal :: ServiceConfig -> Text -> Method -> MatchResult
matchNormal svc path method =
  let routes = serviceRoutes svc
      -- Find explicit routes matching the path (any method)
      pathMatches = [ (route, params)
                    | route <- routes
                    , Just params <- [matchPattern (routePath route) path]
                    ]
      -- Among path matches, find those with the right method
      fullMatches = [ (route, params)
                    | (route, params) <- pathMatches
                    , routeMethod route == method
                    ]
      blanket = allowedMethods svc
   in case fullMatches of
        ((route, params) : _) -> RouteMatched svc route params
        [] | not (null pathMatches) && method `notElem` blanket ->
               -- Path matched an explicit route but wrong method, and no blanket covers it
               MethodNotAllowed path
           | method `elem` blanket ->
               -- Blanket match
               RouteMatched svc (synthesizeBlanketRoute (serviceName svc) (allowedMethodsBasePath svc) path method) []
           | not (null blanket) ->
               -- Blanket exists but method not in it
               MethodNotAllowed path
           | otherwise -> NoRouteFound

-- | Denylist mode: deny explicitly listed routes, allow everything else within allowedMethods.
matchInverted :: ServiceConfig -> Text -> Method -> MatchResult
matchInverted svc path method
  | method `notElem` allowedMethods svc = MethodNotAllowed path
  | otherwise =
      let routes = serviceRoutes svc
          -- Check if this path+method is on the denylist
          denied = [ route
                   | route <- routes
                   , Just _ <- [matchPattern (routePath route) path]
                   , routeMethod route == method
                   ]
       in case denied of
            (_ : _) -> RouteDenied path
            []      -> RouteMatched svc (synthesizeBlanketRoute (serviceName svc) (allowedMethodsBasePath svc) path method) []

-- | Synthesize a RouteConfig for blanket matches (no explicit route).
-- Strips the /<serviceName> prefix from the path and prepends the basePath to derive the target path.
synthesizeBlanketRoute :: Text -> Text -> Text -> Method -> RouteConfig
synthesizeBlanketRoute svcName basePath path method =
  let stripped = stripServicePrefix svcName path
      target = if T.null basePath then stripped else basePath <> stripped
   in RouteConfig
        { routePath = path
        , routeTargetPath = target
        , routeMethod = method
        , routeScripts = ScriptChain [] []
        }

-- | Strip the /<serviceName> prefix from a path.
-- e.g., stripServicePrefix "jira" "/jira/rest/api/3/search" = "/rest/api/3/search"
stripServicePrefix :: Text -> Text -> Text
stripServicePrefix svcName path =
  let prefix = "/" <> svcName
      stripped = T.drop (T.length prefix) path
   in if T.null stripped then "/" else stripped

-- | Match a route pattern against a request path, capturing {param} segments.
-- Returns Nothing if the pattern does not match.
-- Segments are split on '/', empty segments are filtered out.
-- A pattern segment like "{id}" matches any non-empty path segment and captures it.
-- A literal segment must match exactly.
matchPattern :: Text -> Text -> Maybe PathParams
matchPattern pattern path =
  let patSegs = filter (not . T.null) (T.splitOn "/" pattern)
      pathSegs = filter (not . T.null) (T.splitOn "/" path)
   in if length patSegs /= length pathSegs
        then Nothing
        else go patSegs pathSegs []
  where
    go [] [] acc = Just (reverse acc)
    go (p : ps) (s : ss) acc
      | isParam p =
          let name = T.drop 1 (T.dropEnd 1 p)
           in go ps ss ((name, s) : acc)
      | p == s = go ps ss acc
      | otherwise = Nothing
    go _ _ _ = Nothing

    isParam seg = T.isPrefixOf "{" seg && T.isSuffixOf "}" seg
