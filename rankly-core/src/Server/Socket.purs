module Server.Socket where

import Prelude
import Effect (Effect)
import Node.HTTP (Server)

foreign import data SocketIO :: Type

foreign import data Socket :: Type

foreign import attachSocketIO :: Server -> Effect SocketIO

foreign import on :: SocketIO -> String -> (Socket -> Effect Unit) -> Effect Unit