module Main where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Console as Console
import Effect.Aff (Aff, launchAff_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Tuple.Nested (type (/\), (/\))
import Data.Traversable (for_)
import Data.Filterable (class Filterable, filter)

import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Transmission as Transmission

import Y.Server.Util.Relation (Relation)
import Y.Server.Util.Relation as Relation
import Y.Server.Postgres as Postgres
import Y.Server.ServerConfig (getServerConfig, SslConfig(..))
import Y.Server.WebSocket as Ws
import Y.Server.Persist as Persist

type State =
  { db :: Postgres.Database
  , rooms :: Map (Id "Room") Room
  , clients :: Map (Id "Client") Client
  , subs :: Relation (Id "Client") (Id "Room")
  -- ^ Relates clients to the rooms they are subscribed to
  , clientUserIds :: Map (Id "Client") (Id "User")
  -- ^ Relates clients to the user id of the person piloting the client
  }

type Room =
  { id :: Id "Room"
  , events :: Sorted Array Event
  }

type Client =
  { id :: Id "Client"
  , wsClient :: Ws.Client Transmission.ToServer Transmission.ToClient
  }

mkState0 :: Postgres.Database -> State
mkState0 db =
  { db: db
  , rooms: Map.empty
  , clients: Map.empty
  , subs: Relation.empty
  , clientUserIds: Map.empty
  }

-- | Which clients are interested in the given event?
getInterestedClients :: forall l. Filterable l => State -> Event -> l Client -> l Client
getInterestedClients state (Event event) universe =
  case event.payload of
    EventPayload_SetName _ -> universe
    EventPayload_MessageSend _ -> universe
    EventPayload_MessageEdit _ -> universe
    EventPayload_MessageDelete _ -> universe
    EventPayload_MessageSetIsUnread { userId } ->
      let isInterested client = state.clientUserIds # Map.lookup client.id # map (_ == userId) # fromMaybe false
      in filter isInterested universe

onTransmission :: Client -> Transmission.ToServer -> State -> Aff State
onTransmission = \client tn -> case tn of
  Transmission.ToServer_Hello { userId } -> \state -> do
    pure $ state { clientUserIds = state.clientUserIds # Map.insert client.id userId }

  Transmission.ToServer_Subscribe { roomId } -> \state -> do
    pure $ state { subs = state.subs # Relation.incl client.id roomId }

  Transmission.ToServer_Pull { roomId } -> \state -> do
    room /\ state' <- state # loadRoom roomId
    liftEffect $ client.wsClient # Ws.transmit (Transmission.ToClient_Broadcast $ Array.fromFoldable room.events)
    pure state'

  Transmission.ToServer_Push { roomId, event } -> \state -> do
    -- v Log event in db
    state.db # Persist.insertEvent event

    -- v Notify interested clients
    let interested = getInterestedClients state event (Map.values state.clients)
    liftEffect $ for_ interested $ _.wsClient >>> Ws.transmit (Transmission.ToClient_Broadcast $ Array.singleton event)

    -- v Log event in memory
    room /\ state' <- state # loadRoom roomId
    let room' = room { events = room.events # Sorted.insert event }
    let state'' = state' { rooms = state'.rooms # Map.insert roomId room' }
    pure state''

  where

  -- | Retrieve a room from the state
  -- | If it doesn't exist, initialize it
  loadRoom :: Id "Room" -> State -> Aff (Room /\ State)
  loadRoom roomId state =
    case Map.lookup roomId state.rooms of
      Just room -> pure $ room /\ state
      Nothing -> do
        events <- state.db # Persist.retrieveEvents roomId
        let (room :: Room) = { id: roomId, events }
        let state' = state { rooms = state.rooms # Map.insert roomId room }
        pure $ room /\ state'

onClose :: Client -> State -> Effect State
onClose client state = do
  Console.log $ "End WebSocket connection: " <> Id.format client.id
  pure $ state
    { clients = state.clients # Map.delete client.id
    , subs = state.subs # Relation.lexp client.id
    , clientUserIds = state.clientUserIds # Map.delete client.id
    }

main :: Effect Unit
main = launchAff_ do

  liftEffect $ Console.log "Running"

  config <- liftEffect getServerConfig
  db <- Persist.open config
  stateRef <- liftEffect $ Ref.new (mkState0 db)
  Persist.migrate config db

  wsServer <- liftEffect $
    case config.sslConfig of
      SslConfig_NoSsl -> Ws.newServer_http { port: 8081 }
      SslConfig_UseSsl sslInfo -> Ws.newServer_https { port: 8081, sslInfo }

  liftEffect $ wsServer # Ws.onConnection \wsClient -> do
    clientId <- Id.new
    let (client :: Client) = { id: clientId, wsClient }
    stateRef # Ref.modify_ \state -> state { clients = state.clients # Map.insert clientId client }

    Console.log $ "New WebSocket connection: " <> Id.format clientId

    client.wsClient # Ws.onTransmission \maybeTn -> launchAff_ case maybeTn of
      Left err -> liftEffect $ Console.warn $ "Warning: transmission failed to decode; details:\n" <> err
      Right tn -> stateRef # refModifyM_ (onTransmission client tn)

    client.wsClient # Ws.onClose do stateRef # refModifyM_ (onClose client)

  where

  refModifyM_ :: forall m a. MonadEffect m => (a -> m a) -> Ref a -> m Unit
  refModifyM_ f ref = liftEffect (Ref.read ref) >>= f >>= (\x -> liftEffect $ Ref.write x ref)
