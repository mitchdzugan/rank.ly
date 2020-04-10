module Scroll where

import Prelude
import Effect

foreign import getTopId :: Effect String

foreign import scrollToId :: String -> Effect Unit

foreign import attachScrollWatch :: (String -> Effect Unit) -> Effect (Effect Unit)
