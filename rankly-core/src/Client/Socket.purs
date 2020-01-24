module Client.Socket where

import Prelude
import Effect (Effect)

foreign import data Socket :: Type

foreign import connect :: Effect Socket

foreign import on :: Socket -> String -> Effect Unit -> Effect Unit
