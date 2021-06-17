module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold)
import Effect.Class (liftEffect)

import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Transmission as Transmission

import Y.Client.App (runApp)
import Y.Client.Core (mkInitialModel, Y_Ws_Client)
import Y.Client.Action (Action(..), runAction)
import Y.Client.Actions as Actions
import Y.Client.View (view)
import Y.Client.WebSocket as Ws
import Y.Client.ToSub (websocketClientToSub, MorallySub, morallySubToSub)

foreign import initialize_f :: forall a b r.
  (Id a -> Id b -> r) ->
  Id a -> Id b -> Effect r

foreign import getWsTarget :: Effect String
foreign import workaround_redirectFocusFromBodyToRoot :: Effect Unit
foreign import screenDimsMorallySub :: MorallySub { width :: Number, height :: Number }

main :: Effect Unit
main = do

  -- Initialize state
  freshUserId /\ freshConvoId <- (/\) <$> Id.new <*> Id.new
  userId /\ convoId <- initialize_f (/\) freshUserId freshConvoId
  let initialModel = mkInitialModel userId convoId

  -- Spin up websocket
  wsTarget <- getWsTarget
  (wsClient :: Y_Ws_Client) <- Ws.newConnection { url: wsTarget }

  -- apply hacky workaround regarding element focus
  workaround_redirectFocusFromBodyToRoot

  -- set up subscriptions
  let subs = fold
        [ websocketClientToSub wsClient
            # map case _ of
                Nothing -> Action \model ->
                  model <$ liftEffect (Console.warn "Events list failed to parse; doing nothing")
                Just (Transmission.ToClient_Broadcast events) -> events # map Actions.fromEvent # fold

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
    wsClient # Ws.transmit (Transmission.ToServer_Subscribe { userId, convoId })
    wsClient # Ws.transmit (Transmission.ToServer_Pull { convoId })
