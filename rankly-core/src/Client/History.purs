module Client.History where

import Prelude
import Effect (Effect)
import Impulse.FRP

foreign import init
  :: forall a.
     (SigBuilder a -> Effect { destroy :: Effect Unit, signal :: Signal a }) ->
     (Event a -> a -> SigBuilder a) ->
     (a -> Event a -> Effect Unit) ->
     Event a ->
     (String -> a) ->
     Effect (Signal a)

foreign import push :: String -> Effect Unit
