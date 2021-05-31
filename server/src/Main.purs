module Main where

import Prelude

import Debug as Debug

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Console as Console
import Data.Maybe (Maybe(..), fromJust)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.List (List)
import Data.List as List
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_)
import Data.Monoid (guard)
import Partial.Unsafe (unsafePartial)

import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Transmission (Transmission(..))
import Y.Shared.Config as Config

import Y.Server.Util.Relation (Relation)
import Y.Server.Util.Relation as Relation
import Y.Server.WebSocket as Ws

type Convo =
  { id :: Id "Convo"
  , events :: List Event  -- expected to be sorted by time
  }

type Client = Ws.Client Transmission (List Event)

-- | Relates clients to the conversations that they are subscribed to
type Subs = Relation (Id "Client") (Id "Convo")

-- | Keep track of...
type Convos = Map (Id "Convo") Convo  -- ongoing conversations
type Clients = Map (Id "Client") Client  -- connected clients
type UserIds = Map (Id "Client") (Id "User")  -- client user ids

-- TODO: this setup is... ugly.

_convos_ensureExists :: Id "Convo" -> Ref Convos -> Effect Unit
_convos_ensureExists convoId convosRef = do
  existing <- Ref.read convosRef <#> Map.lookup convoId
  case existing of
    Just _ -> pure unit
    Nothing -> do
      let convo = { id: convoId, events: mempty }
      convosRef # Ref.modify_ (Map.insert convo.id convo)
      pure unit

convos_get :: Id "Convo" -> Ref Convos -> Effect Convo
convos_get convoId convosRef = do
  _convos_ensureExists convoId convosRef
  unsafePartial $ Ref.read convosRef <#> Map.lookup convoId >>> fromJust

convos_modify :: Id "Convo" -> (Convo -> Convo) -> Ref Convos -> Effect Unit
convos_modify convoId f convosRef = do
  _convos_ensureExists convoId convosRef
  convosRef # Ref.modify_ \convos ->
    let convo = unsafePartial $ Map.lookup convoId convos # fromJust
    in Map.insert convoId (f convo) convos

main :: Effect Unit
main = do

  subsRef <- Ref.new (Relation.empty :: Subs)
  convosRef <- Ref.new (Map.empty :: Convos)
  clientsRef <- Ref.new (Map.empty :: Clients)
  userIdsRef <- Ref.new (Map.empty :: UserIds)

  server <- Ws.newServer { port: Config.webSocketPort }

  server # Ws.onConnection \client -> do

    clientId <- Id.new
    clientsRef # Ref.modify_ (Map.insert clientId client)

    Console.log $ "New WebSocket connection: " <> Id.format clientId

    client # Ws.onTransmission case _ of
      Left err -> Console.warn $ "Warning: transmission failed to decode; details:\n" <> err
      Right tn -> do
        case tn of
          Transmission_Subscribe { userId, convoId } -> do
            subsRef # Ref.modify_ (Relation.incl clientId convoId)
            userIdsRef # Ref.modify_ (Map.insert clientId userId)

          Transmission_Pull { convoId } -> do
            convo <- convos_get convoId convosRef
            let events = (Debug.log convo).events
            client # Ws.transmit events

          Transmission_Push { convoId, event } -> do
            -- v Log event
            convosRef # convos_modify convoId \convo -> convo { events = convo.events <> List.singleton event }
            -- v Notify interested clients
            subbedClientIds :: Set (Id "Client") <- Ref.read subsRef <#> Relation.rget convoId
            clientUserId <- Ref.read userIdsRef <#> Map.lookup clientId
            let (interestedClientIds :: Array (Id "Client")) = Set.toUnfoldable $
                  case event of
                      Event { payload: EventPayload_SetName _ } -> subbedClientIds
                      Event { payload: EventPayload_MessageSend _ } -> subbedClientIds
                      Event { payload: EventPayload_SetReadState { userId } } ->
                        guard (clientUserId == Just userId) $ Set.singleton clientId
            for_ subbedClientIds \subbedClientId -> do
              clients <- Ref.read clientsRef
              let (subbed :: Client) = unsafePartial $ fromJust $ Map.lookup subbedClientId clients
              subbed # Ws.transmit (List.singleton event)

    client # Ws.onClose do
      subsRef # Ref.modify_ (Relation.lexp clientId)
      Console.log $ "End WebSocket connection: " <> Id.format clientId
