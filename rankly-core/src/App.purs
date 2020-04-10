module App where

import Prelude
import Data.Symbol (SProxy(..))
import Effect
import Impulse.DOM
import Impulse.FRP as FRP
import Route (Route)

p_navigate = SProxy :: SProxy "navigate"
p_route = SProxy :: SProxy "route"

type AppDOM e c a = DOM { route :: FRP.Signal Route
                        | e
                        }
                        { navigate :: Collector Route | c }
                        a

type AppEnv = { pushRoute :: Route -> Effect Unit
              , s_route :: FRP.Signal Route
              }
mkAppEnv :: FRP.Signal Route -> (Route -> Effect Unit) -> AppEnv
mkAppEnv s_route pushRoute =
  { s_route, pushRoute }

mkApp ::
  forall e c.
  AppEnv ->
  AppDOM ( | e ) ( | c ) Unit ->
  DOM { | e } { | c } Unit
mkApp { pushRoute, s_route } app = do
  e_collect p_navigate $ \e_route -> do
    _ <- _eff $ FRP.consume pushRoute e_route
    upsertEnv p_route s_route do
      app
