module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Effect.Console as Console
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))
import Control.Monad.Trans.Class (lift)

import Platform (Program)
import Platform as Platform
import Sub (Sub)

import Shared.Id (Id, newId)
import Shared.Convo (Event)
import Shared.Transmission (Transmission(..))
import Shared.Config as Config

import Client.Core (Model, Msg)
import Client.View (view)
import Client.WebSocket as Ws
import Client.WebSocketClientToElmishSubscription (websocketClientToElmishSubscription)

main :: Effect Unit
main = do

  -- Initialize state
  { uid, cid } <- initialize

  -- Spin up websocket
  hostname <- getHostname
  (wsClient :: Ws.Client Transmission (List Event))
    <- Ws.newConnection { url: "ws://" <> hostname <> ":" <> show Config.webSocketPort }

  -- Start Elmish
  let sub = mkSub wsClient
  let app = mkApp uid cid sub
  (runEffectFn1 app) unit

  -- Kick the thing off!
  wsClient # Ws.onOpen do
    Console.log "WebSocket opened"
    wsClient # Ws.transmit (Transmission_Subscribe { cid })
    wsClient # Ws.transmit (Transmission_Pull { cid })

  where
    mkSub :: forall ts. Ws.Client ts (List Event) -> Sub Msg
    mkSub = websocketClientToElmishSubscription >>> map maybeEventsToMsg

    maybeEventsToMsg :: Maybe (List Event) -> Msg
    maybeEventsToMsg = case _ of
      Nothing -> \model -> Console.warn "Events list failed to parse; doing nothing" $> model
      Just events -> \model -> pure $ model { convo = model.convo { events = model.convo.events <> events } }

foreign import getHostname :: Effect String

initialize :: Effect { uid :: Id "User", cid :: Id "Convo" }
initialize = initialize_f (\n -> newId { namespace: n }) (\uid cid -> { uid, cid })

foreign import initialize_f
  :: forall a b ab.
     (forall for. String -> Effect (Id for)) -> (Id a -> Id b -> ab)
  -> Effect ab

mkApp :: Id "User" -> Id "Convo" -> Sub Msg -> Program Unit Model Msg
mkApp uid cid sub = Platform.app
    { init: pure <<< const model0
    , update: \model msg -> lift (msg model)
    , subscriptions: const sub
    , view
    }

  where
    model0 =
      { userId: uid
      , convo:
        { id: cid
        , events: List.Nil
        }
      }

