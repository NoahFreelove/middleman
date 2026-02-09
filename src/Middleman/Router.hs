module Middleman.Router
  ( MatchResult (..)
  , matchRoute
  , matchPattern
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Middleman.Types
  ( GlobalConfig (..)
  , PathParams
  , RouteConfig (..)
  , ServiceConfig (..)
  )
import Network.HTTP.Types (Method)

-- | Result of matching an incoming request to a route
data MatchResult
  = RouteMatched ServiceConfig RouteConfig PathParams
  | NoRouteFound
  | MethodNotAllowed Text
  deriving (Show)

-- | Match an incoming request path and method to a route.
-- Pattern-based matching with {param} captures, first-match-wins across all services.
matchRoute :: GlobalConfig -> Text -> Method -> MatchResult
matchRoute cfg path method =
  case findByPath path (globalServices cfg) of
    [] -> NoRouteFound
    candidates ->
      case filter (\(_, r, _) -> routeMethod r == method) candidates of
        ((svc, route, params) : _) -> RouteMatched svc route params
        [] -> MethodNotAllowed path

-- | Find all (service, route, params) triples where the route pattern matches the path
findByPath :: Text -> [ServiceConfig] -> [(ServiceConfig, RouteConfig, PathParams)]
findByPath path services =
  [ (svc, route, params)
  | svc <- services
  , route <- serviceRoutes svc
  , Just params <- [matchPattern (routePath route) path]
  ]

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
