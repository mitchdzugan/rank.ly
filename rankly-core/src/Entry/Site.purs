module Entry.Site where

import Prelude
import App
import Route
import Impulse.DOM
import Impulse.FRP
import Impulse.FRP as FRP
import UI.ImpulseGuide as ImpulseGuide
import UI.Resume as Resume

router Home = div_ (className "container") do
  p_ anil $ text "Under Construction until I figure out what I want to put here..."
  p_ anil do
    text "For now check out existing projects on "
    a_ (href $ toUrl Resume) $ text "My Resume"
    text " or learn about my web framework with the "
    a_ (href $ toUrl Impulse) $ text "Impulse Developer Guide"
router Resume = Resume.resume
router Impulse = ImpulseGuide.impulseGuide
router Error = text "Error 404"

entry :: AppDOM () () Unit
entry = do
  route <- getEnv p_route
  s_bindDOM_ route router
