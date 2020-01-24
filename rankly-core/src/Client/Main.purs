module Client.Main where

import Prelude hiding (div)
import Client.History
import Effect (Effect)
import Effect.Console (log)
import Impulse.FRP as FRP
import Impulse.DOM
import Route
import Data.Either
import Client.Socket as Socket
import Data.Maybe
import UI.Entry.Site (entry)
import App

main :: Effect Unit
main = do
  s_route <- init parseAnyRoute
  let pushRoute = \route -> push $ toUrl route
      e_socket = FRP.mkEvent \_ -> pure $ pure unit
  sb_socketConnection <- FRP.eff_sigBuilder $ FRP.s_from e_socket Waiting
  socket <- Socket.connect
  Socket.on socket "connect" do
    flip FRP.push e_socket $ Connected socket
  Socket.on socket "disconnect" do
    flip FRP.push e_socket Disconnected
  _ <- attach "app" {} $ flip mkApp entry
                       $ sb_socketConnection.signal # mkAppEnv s_route pushRoute
  pure unit

