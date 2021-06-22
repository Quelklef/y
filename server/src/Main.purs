module Main where

import Prelude

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
import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for_)
import Data.Monoid (guard)
import Partial.Unsafe (unsafePartial)

import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Transmission as Transmission

import Y.Server.Util.Relation (Relation)
import Y.Server.Util.Relation as Relation
import Y.Server.ServerConfig (getServerConfig, SslConfig(..))
import Y.Server.WebSocket as Ws

type Room =
  { id :: Id "Room"
  , events :: List Event  -- expected to be sorted by time
  }

type Client = Ws.Client Transmission.ToServer Transmission.ToClient

-- | Relates clients to the conversations that they are subscribed to
type Subs = Relation (Id "Client") (Id "Room")

-- | Keep track of...
type Rooms = Map (Id "Room") Room  -- ongoing conversations
type Clients = Map (Id "Client") Client  -- connected clients
type UserIds = Map (Id "Client") (Id "User")  -- client user ids

-- TODO: this setup is... ugly.

_rooms_ensureExists :: Id "Room" -> Ref Rooms -> Effect Unit
_rooms_ensureExists roomId roomsRef = do
  existing <- Ref.read roomsRef <#> Map.lookup roomId
  case existing of
    Just _ -> pure unit
    Nothing -> do
      let room = { id: roomId, events: mempty }
      roomsRef # Ref.modify_ (Map.insert room.id room)
      pure unit

rooms_get :: Id "Room" -> Ref Rooms -> Effect Room
rooms_get roomId roomsRef = do
  _rooms_ensureExists roomId roomsRef
  unsafePartial $ Ref.read roomsRef <#> Map.lookup roomId >>> fromJust

rooms_modify :: Id "Room" -> (Room -> Room) -> Ref Rooms -> Effect Unit
rooms_modify roomId f roomsRef = do
  _rooms_ensureExists roomId roomsRef
  roomsRef # Ref.modify_ \rooms ->
    let room = unsafePartial $ Map.lookup roomId rooms # fromJust
    in Map.insert roomId (f room) rooms

main :: Effect Unit
main = do

  config <- getServerConfig

  Console.log "Running"

  subsRef <- Ref.new (Relation.empty :: Subs)
  roomsRef <- Ref.new (Map.empty :: Rooms)
  clientsRef <- Ref.new (Map.empty :: Clients)
  userIdsRef <- Ref.new (Map.empty :: UserIds)

  server <-
    case config.sslConfig of
      SslConfig_NoSsl -> Ws.newServer_http { port: 8081 }
      SslConfig_UseSsl sslInfo -> Ws.newServer_https { port: 8081, sslInfo }

  server # Ws.onConnection \client -> do

    clientId <- Id.new
    clientsRef # Ref.modify_ (Map.insert clientId client)

    Console.log $ "New WebSocket connection: " <> Id.format clientId

    client # Ws.onTransmission case _ of
      Left err -> Console.warn $ "Warning: transmission failed to decode; details:\n" <> err
      Right tn -> do
        case tn of
          Transmission.ToServer_Subscribe { userId, roomId } -> do
            subsRef # Ref.modify_ (Relation.incl clientId roomId)
            userIdsRef # Ref.modify_ (Map.insert clientId userId)

          Transmission.ToServer_Pull { roomId } -> do
            room <- rooms_get roomId roomsRef
            let events = Array.fromFoldable room.events
            client # Ws.transmit (Transmission.ToClient_Broadcast events)

          Transmission.ToServer_Push { roomId, event } -> do
            -- v Log event
            roomsRef # rooms_modify roomId \room -> room { events = room.events <> List.singleton event }
            -- v Notify interested clients
            subbedClientIds :: Set (Id "Client") <- Ref.read subsRef <#> Relation.rget roomId
            clientUserId <- Ref.read userIdsRef <#> Map.lookup clientId
            let (interestedClientIds :: Array (Id "Client")) = Set.toUnfoldable $
                  case event of
                      Event { payload: EventPayload_SetName _ } -> subbedClientIds
                      Event { payload: EventPayload_MessageSend _ } -> subbedClientIds
                      Event { payload: EventPayload_MessageEdit _ } -> subbedClientIds
                      Event { payload: EventPayload_MessageDelete _ } -> subbedClientIds
                      Event { payload: EventPayload_SetReadState { userId } } ->
                        guard (clientUserId == Just userId) $ Set.singleton clientId
            for_ interestedClientIds \interestedClientId -> do
              clients <- Ref.read clientsRef
              let (interested :: Client) = unsafePartial $ fromJust $ Map.lookup interestedClientId clients
              interested # Ws.transmit (Transmission.ToClient_Broadcast $ Array.singleton event)

    client # Ws.onClose do
      subsRef # Ref.modify_ (Relation.lexp clientId)
      Console.log $ "End WebSocket connection: " <> Id.format clientId
