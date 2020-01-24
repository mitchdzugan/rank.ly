module Route where

import Data.Either
import Prelude

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root, segment, int, parse, print)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteError)
import Data.Maybe

data RankingRoute
  = All
  | Create
  | Show Int
  | Clone Int
derive instance genericRankingRoute :: Generic RankingRoute _
rankingRouteSpec :: RouteDuplex' RankingRoute
rankingRouteSpec = G.sum
  { "All": G.noArgs
  , "Create": "create" / G.noArgs
  , "Show": int segment
  , "Clone": int segment / "clone"
  }


data UserRoute
  = View Int
  | ViewRanking Int Int
derive instance genericUserRoute :: Generic UserRoute _
userRouteSpec :: RouteDuplex' UserRoute
userRouteSpec = G.sum
  { "View": int segment
  , "ViewRanking": int segment / int segment
  }


data Route
  = Home
  | Search
  | Login
  | Register
  | User UserRoute
  | Ranking RankingRoute
  | Error
derive instance genericRoute :: Generic Route _
routeSpec :: RouteDuplex' Route
routeSpec = root $ G.sum
  { "Home": G.noArgs
  , "Search": "search" / G.noArgs
  , "Login": "login" / G.noArgs
  , "Register": "register" / G.noArgs
  , "User": "user" / userRouteSpec
  , "Ranking": "ranking" / rankingRouteSpec
  , "Error": G.noArgs
  }

parseRoute :: String -> Either RouteError Route
parseRoute = parse routeSpec

parseAnyRoute :: String -> Route
parseAnyRoute path = case (parseRoute path) of
  Right route -> route
  Left _ -> Error

toUrl :: Route -> String
toUrl = print routeSpec

instance eqRoute :: Eq Route where
  eq r1 r2 = toUrl r1 == toUrl r2

instance showRoute :: Show Route where
  show = toUrl
