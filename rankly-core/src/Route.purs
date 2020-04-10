module Route where

import Data.Either
import Prelude

import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', path, root, segment, int, parse, print)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax ((/), (?))
import Routing.Duplex.Parser (RouteError)
import Data.Maybe

data Route
  = Home
  | Resume
  | Impulse
  | Error
derive instance genericRoute :: Generic Route _
routeSpec :: RouteDuplex' Route
routeSpec = root $ G.sum
  { "Home": G.noArgs
  , "Resume": "resume" / G.noArgs
  , "Impulse": "impulse" / G.noArgs
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
