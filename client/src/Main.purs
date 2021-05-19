module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)

import Sub (Sub)

import Y.Shared.Id (Id, newId)
import Y.Shared.Convo (Event)
import Y.Shared.Transmission (Transmission(..))
import Y.Shared.Config as Config

import Y.Client.App (runApp)
import Y.Client.Core (mkInitialModel)
import Y.Client.Action (Action, runActionMonad)
import Y.Client.View (view)
import Y.Client.WebSocket as Ws
import Y.Client.WebSocketClientToElmishSubscription (websocketClientToElmishSubscription)

main :: Effect Unit
main = do

  -- Initialize state
  let initialModel = mkInitialModel

  -- Start Elmish
  runApp
    { initialModel: initialModel
    , subscriptions: const mempty
    , view: view
    , interpret: identity
    }

