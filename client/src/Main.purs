module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)

import Sub (Sub)

import Shared.Id (Id, newId)
import Shared.Convo (Event)
import Shared.Transmission (Transmission(..))
import Shared.Config as Config

import Client.App (runApp)
import Client.Core (mkInitialModel)
import Client.Action (Action, runActionMonad)
import Client.View (view)
import Client.WebSocket as Ws
import Client.WebSocketClientToElmishSubscription (websocketClientToElmishSubscription)

foreign import initialize_f :: forall a b r.
  (Id a -> Id b -> r) ->
  Id a -> Id b -> Effect r

foreign import getHostname :: Effect String

main :: Effect Unit
main = do

  -- Initialize state
  freshUserId /\ freshConvoId <- (/\) <$> newId <*> newId
  userId /\ convoId <- initialize_f (/\) freshUserId freshConvoId
  let initialModel = mkInitialModel userId convoId

  -- Spin up websocket
  hostname <- getHostname
  (wsClient :: Ws.Client Transmission (List Event))
    <- Ws.newConnection { url: "ws://" <> hostname <> ":" <> show Config.webSocketPort }

  -- Start Elmish
  let sub = mkSub wsClient
  runApp
    { initialModel: initialModel
    , subscriptions: const sub
    , view: view
    , interpret: runActionMonad { wsClient }
    }

  -- Kick the thing off!
  wsClient # Ws.onOpen do
    Console.log "WebSocket opened"
    wsClient # Ws.transmit (Transmission_Subscribe { convoId })
    wsClient # Ws.transmit (Transmission_Pull { convoId })

  where
    mkSub :: forall ts. Ws.Client ts (List Event) -> Sub Action
    mkSub = websocketClientToElmishSubscription >>> map maybeEventsToAction

    maybeEventsToAction :: Maybe (List Event) -> Action
    maybeEventsToAction = case _ of
      Nothing -> \model -> model <$ liftEffect (Console.warn "Events list failed to parse; doing nothing")
      Just events -> \model -> pure $ model { convo = model.convo { events = model.convo.events <> events } }
