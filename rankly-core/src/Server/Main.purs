module Server.Main where

import Debug.Trace
import Prelude
import Data.Array ((!!))
import Data.Either (Either (..))
import Data.Maybe
import Data.String (Pattern (..), split)
import Data.String.Regex.Flags (noFlags)
import Node.Encoding
import Data.String.Regex.Unsafe (unsafeRegex)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (makeAff, effectCanceler)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import Impulse.DOM (toMarkup, ssr_then)
import Impulse.FRP as FRP
import Node.Express.App (App, listenHttp, get, use)
import Node.Express.Handler (next)
import Node.Express.Middleware.Static (static)
import Node.Express.Request (getPath)
import Node.Express.Response (send)
import Node.FS.Sync (readTextFile)
import Node.HTTP (Server)
import Server.Socket
import Route
import UI.Entry.Site (entry)
import App

pushRoute :: Route -> Effect Unit
pushRoute _ = pure unit

app :: String -> String -> App
app pre post = do
  -- Static routes
  get "/favicon.ico" do
    pure unit
  -- Main route
  get (unsafeRegex """.*""" noFlags) do 
    route <- getPath
    case (parseRoute route) of
      Right route -> do
        liftEffect $ log "1"
        sb_route <- liftEffect $ FRP.eff_sigBuilder $ FRP.s_const route
        liftEffect $ log "2"
        sb_socketConnection <- liftEffect $ FRP.eff_sigBuilder $ FRP.s_const Waiting
        liftEffect $ log "3"
        trace { route } \_ -> pure unit
        ssr <- liftEffect $ toMarkup {} $ flip mkApp entry
                                        $ mkAppEnv sb_route.signal
                                                   pushRoute
                                                   sb_socketConnection.signal
        liftEffect $ log "4"
        markup <- liftAff $ makeAff \resolve -> do
          ssr_then ssr \markup _ -> resolve $ Right markup
          pure $ effectCanceler $ pure unit
        liftEffect $ log "5"
        liftEffect $ sb_route.destroy
        liftEffect $ sb_socketConnection.destroy
        send $ pre <> markup <> post
      Left _ -> do
        next
  -- everything else
  use $ static "dist"
  -- invalid route error page
  get (unsafeRegex """.*""" noFlags) do
    sb_route <- liftEffect $ FRP.eff_sigBuilder $ FRP.s_const Error
    sb_socketConnection <- liftEffect $ FRP.eff_sigBuilder $ FRP.s_const Waiting
    ssr <- liftEffect $ toMarkup {} $ flip mkApp entry
                                    $ mkAppEnv sb_route.signal
                                               pushRoute
                                               sb_socketConnection.signal
    markup <- liftAff $ makeAff \resolve -> do
      ssr_then ssr \markup _ -> resolve $ Right markup
      pure $ effectCanceler $ pure unit
    liftEffect $ sb_route.destroy
    liftEffect $ sb_socketConnection.destroy
    send $ pre <> markup <> post

main :: Effect Unit
main = do
  indexHtml <- readTextFile UTF8 "./dist/index.html"
  let splits = split (Pattern "__SSR_CONTENT__") indexHtml
      pre  = fromMaybe "" $ splits !! 0
      post = fromMaybe "" $ splits !! 1
  server <- listenHttp (app pre post) 8080 \_ -> log $ "Listening on " <> show 8080
  io <- attachSocketIO server
  on io "connection" \_ -> do
    log "Client connected"
  pure unit
