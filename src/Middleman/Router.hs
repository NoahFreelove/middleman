module Middleman.Router
  ( MatchResult (..)
  , matchRoute
  ) where

import Data.Text (Text)
import Middleman.Types
  ( GlobalConfig (..)
  , RouteConfig (..)
  , ServiceConfig (..)
  )
import Network.HTTP.Types (Method)

-- | Result of matching an incoming request to a route
data MatchResult
  = RouteMatched ServiceConfig RouteConfig
  | NoRouteFound
  | MethodNotAllowed Text
  deriving (Show)

-- | Match an incoming request path and method to a route.
-- Exact path matching, first-match-wins across all services.
matchRoute :: GlobalConfig -> Text -> Method -> MatchResult
matchRoute cfg path method =
  case findByPath path (globalServices cfg) of
    [] -> NoRouteFound
    candidates ->
      case filter (\(_, r) -> routeMethod r == method) candidates of
        ((svc, route) : _) -> RouteMatched svc route
        [] -> MethodNotAllowed path

-- | Find all (service, route) pairs where the route path matches
findByPath :: Text -> [ServiceConfig] -> [(ServiceConfig, RouteConfig)]
findByPath path services =
  [ (svc, route)
  | svc <- services
  , route <- serviceRoutes svc
  , routePath route == path
  ]
