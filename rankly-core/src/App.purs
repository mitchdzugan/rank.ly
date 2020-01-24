module App where

import Prelude
import Client.Socket (Socket)
import Data.Symbol (SProxy(..))
import Effect
import Impulse.DOM
import Impulse.FRP as FRP
import Route (Route)

p_navigate = SProxy :: SProxy "navigate"
p_route = SProxy :: SProxy "route"
p_socketConnection = SProxy :: SProxy "socketConnection"

data SocketConnection = Waiting
                      | Disconnected
                      | Connected Socket

type AppDOM e c a = DOM { route :: FRP.Signal Route
                        , socketConnection :: FRP.Signal SocketConnection
                        | e
                        }
                        { navigate :: Collector Route | c }
                        a

type AppEnv = { pushRoute :: Route -> Effect Unit
              , s_route :: FRP.Signal Route
              , s_socketConnection :: FRP.Signal SocketConnection
              }
mkAppEnv :: FRP.Signal Route -> (Route -> Effect Unit) -> FRP.Signal SocketConnection -> AppEnv
mkAppEnv s_route pushRoute s_socketConnection =
  { s_route, pushRoute, s_socketConnection }

mkApp ::
  forall e c.
  AppEnv ->
  AppDOM ( | e ) ( | c ) Unit ->
  DOM { | e } { | c } Unit
mkApp { pushRoute, s_route, s_socketConnection } app = do
  e_collect p_navigate $ \e_route -> do
    _ <- _eff $ FRP.consume pushRoute e_route
    upsertEnv p_route s_route do
      upsertEnv p_socketConnection s_socketConnection do
        app
