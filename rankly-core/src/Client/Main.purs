module Client.Main where

import Prelude hiding (div)
import Client.History
import Client.Prism
import Effect (Effect)
import Effect.Console (log)
import Impulse.FRP as FRP
import Impulse.DOM
import Route
import Data.Either
import Data.Maybe
import Entry.Site (entry)
import App

main :: Effect Unit
main = do
  let historyEvent = FRP.mkEvent $ \_ -> pure $ pure unit
  s_route <- init FRP.eff_sigBuilder FRP.s_from FRP.push historyEvent parseAnyRoute
  let pushRoute = \route -> push $ toUrl route
  _ <- attach "app" {} highlightAll
                       $ flip mkApp entry
                       $ mkAppEnv s_route pushRoute
  pure unit

