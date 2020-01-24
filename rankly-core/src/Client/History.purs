module Client.History where

import Prelude
import Effect (Effect)
import Impulse.FRP.Signal (Signal)

foreign import init :: forall a. (String -> a) -> Effect (Signal a)

foreign import push :: String -> Effect Unit
