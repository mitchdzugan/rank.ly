module UI.Entry.Site where

import Prelude
import App
import Route
import Impulse.DOM

entry :: AppDOM () () Unit
entry = do
  div_ (className "section barSpacer") do
    text "Hello!"
