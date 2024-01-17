module Main where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Effect.Console as Console
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.Foldable (fold)
import Effect.Class (liftEffect)
import Data.Functor.Contravariant (cmap)

import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Transmission as Transmission

import Y.Client.App (runApp)
import Y.Client.Core (mkInitialModel, Y_Ws_Client, Model)
import Y.Client.Action (Action (..), AfterRenderEffect (..), runAction)
import Y.Client.Actions as Actions
import Y.Client.View (view)
import Y.Client.WebSocket as Ws
import Y.Client.ToSub (websocketClientToSub, MorallySub, morallySubToSub)

import Mation as M
import Mation.Elems as E
import Mation.Core.Refs (ReadWriteL)
import Mation.Core.Refs as Ref


foreign import localStorage_ ::
  { has :: String -> Effect Boolean
  , get :: String -> Effect String
  , set :: String -> String -> Effect Unit
  }
foreign import getUrlParams_f ::
  (String -> String -> String /\ String) -> (Array (String /\ String) -> Map String String) ->
  Effect (Map String String)
foreign import appendUrlParam :: forall a. String -> String -> Effect a
foreign import getWsTarget :: Effect String
foreign import workaround_redirectFocusFromBodyToRoot :: Effect Unit
foreign import screenDimsMorallySub :: MorallySub { width :: Number, height :: Number }

getUrlParams :: Effect (Map String String)
getUrlParams = getUrlParams_f (/\) Map.fromFoldable

main :: Effect Unit
main = do

  roomId <- do
    let key = "r"
    urlParams <- getUrlParams
    case Map.lookup key urlParams of
      Nothing -> do
        (id :: Id "Room") <- Id.new
        appendUrlParam key (Id.format id)
      Just idStr -> case Id.parse idStr of
        Right id -> pure id
        Left _ -> throw "Bad URL params"

  userId <- do
    let key = "userId"
    exists <- localStorage_.has key
    when (not $ exists) do
      (id :: Id "User") <- Id.new
      localStorage_.set key (Id.format id)
    idStr <- localStorage_.get key
    case Id.parse idStr of
      Right id -> pure id
      Left _ -> throw "Corrupted localStorage"

  let initialModel = mkInitialModel userId roomId

  -- Spin up websocket
  wsTarget <- getWsTarget
  (wsClient :: Y_Ws_Client) <- Ws.newConnection { url: wsTarget }

  {-

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
    , interpret: \action model -> do
        (model' /\ afterRenderEffect) <- runAction { wsClient } action model
        let (AfterRenderEffect e) = afterRenderEffect
        -- TODO kind of a hack, abusing `setTimeout` to move effect to after render
        _ <- setTimeout0 do e
        pure model'
    }

  -}

  M.runApp
    { root: M.underBody
    , initial: initialModel
    , daemon
    , render: view
    }


  -- Kick the thing off!
  wsClient # Ws.onOpen do
    Console.log "WebSocket opened"
    wsClient # Ws.transmit (Transmission.ToServer_Hello { userId })
    wsClient # Ws.transmit (Transmission.ToServer_Subscribe { roomId })
    wsClient # Ws.transmit (Transmission.ToServer_Pull { roomId })

foreign import setTimeout0 :: forall a. Effect a -> Effect Unit
foreign import consoleLog :: forall a. a -> Effect Unit

daemon :: ReadWriteL Model -> Effect Unit
daemon ref = do
  ref # Ref.onChange consoleLog

