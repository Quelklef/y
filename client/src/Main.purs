module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold)
import Effect.Class (liftEffect)

import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event)
import Y.Shared.Transmission (Transmission(..))
import Y.Shared.Config as Config

import Y.Client.App (runApp)
import Y.Client.Core (mkInitialModel)
import Y.Client.Action (Action(..), runAction)
import Y.Client.Actions as Actions
import Y.Client.View (view)
import Y.Client.WebSocket as Ws
import Y.Client.ToSub (websocketClientToSub, MorallySub, morallySubToSub)

foreign import initialize_f :: forall a b r.
  (Id a -> Id b -> r) ->
  Id a -> Id b -> Effect r

foreign import getHostname :: Effect String
foreign import workaround_redirectFocusFromBodyToRoot :: Effect Unit
foreign import screenDimsMorallySub :: MorallySub { width :: Number, height :: Number }

main :: Effect Unit
main = do

  -- Initialize state
  freshUserId /\ freshConvoId <- (/\) <$> Id.new <*> Id.new
  userId /\ convoId <- initialize_f (/\) freshUserId freshConvoId
  let initialModel = mkInitialModel userId convoId

  -- Spin up websocket
  hostname <- getHostname
  (wsClient :: Ws.Client Transmission (List Event))
    <- Ws.newConnection { url: "ws://" <> hostname <> ":" <> show Config.webSocketPort }

  -- apply hacky workaround regarding element focus
  workaround_redirectFocusFromBodyToRoot

  -- set up subscriptions
  let subs = fold
        [ websocketClientToSub wsClient
            # map case _ of
                Nothing -> Action \model ->
                  model <$ liftEffect (Console.warn "Events list failed to parse; doing nothing")
                Just events -> events # map Actions.fromEvent # fold

        , morallySubToSub screenDimsMorallySub
            # map \dims -> Action \model ->
                pure $ model { screenDims = dims }
        ]

  -- Start Elmish
  runApp
    { initialModel: initialModel
    , subscriptions: const subs
    , view: view
    , interpret: runAction { wsClient }
    }

  -- Kick the thing off!
  wsClient # Ws.onOpen do
    Console.log "WebSocket opened"
    wsClient # Ws.transmit (Transmission_Subscribe { convoId })
    wsClient # Ws.transmit (Transmission_Pull { convoId })
