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
import Data.Set as Set
import Data.Traversable (for_)
import Partial.Unsafe (unsafePartial)
import Data.Newtype (unwrap)

import Shared.Id (Id, newId)
import Shared.Convo (Convo, Event)
import Shared.Transmission (Transmission(..))
import Shared.Config as Config

import Server.WebSocket as Ws
import Server.Relation (Relation)
import Server.Relation as Relation

type Client = Ws.Client Transmission (List Event)

-- | Relates clients to the conversations that they are subscribed to
type Subs = Relation (Id "Client") (Id "Convo")

-- | Keep track of ongoing conversations and clients
type Convos = Map (Id "Convo") Convo
type Clients = Map (Id "Client") Client

-- TODO: this setup is... ugly.

_convos_ensureExists :: Id "Convo" -> Ref Convos -> Effect Unit
_convos_ensureExists cid convosRef = do
  existing <- Ref.read convosRef <#> Map.lookup cid
  case existing of
    Just _ -> pure unit
    Nothing -> do
      let convo = { id: cid, events: mempty }
      convosRef # Ref.modify_ (Map.insert convo.id convo)
      pure unit

convos_get :: Id "Convo" -> Ref Convos -> Effect Convo
convos_get cid convosRef = do
  _convos_ensureExists cid convosRef
  unsafePartial $ Ref.read convosRef <#> Map.lookup cid >>> fromJust

convos_modify :: Id "Convo" -> (Convo -> Convo) -> Ref Convos -> Effect Unit
convos_modify cid f convosRef = do
  _convos_ensureExists cid convosRef
  convosRef # Ref.modify_ \convos ->
    let convo = unsafePartial $ Map.lookup cid convos # fromJust
    in Map.insert cid (f convo) convos

main :: Effect Unit
main = do

  subsRef <- Ref.new (Relation.empty :: Subs)
  convosRef <- Ref.new (mempty :: Convos)
  clientsRef <- Ref.new (mempty :: Clients)

  server <- Ws.newServer { port: Config.webSocketPort }

  server # Ws.onConnection \client -> do

    clid <- newId { namespace: "client" }
    clientsRef # Ref.modify_ (Map.insert clid client)

    Console.log $ "New WebSocket connection: " <> unwrap clid

    client # Ws.onTransmission case _ of
      Left err -> Console.warn $ "Warning: transmission failed to decode; details:\n" <> err
      Right tn -> do
        case tn of
          Transmission_Subscribe { cid } -> do
            subsRef # Ref.modify_ (Relation.incl clid cid)

          Transmission_Pull { cid } -> do
            convo <- convos_get cid convosRef
            let events = convo.events
            client # Ws.transmit events

          Transmission_Push { cid, event } -> do
            -- v Push event
            convosRef # convos_modify cid \convo -> convo { events = convo.events <> List.singleton event }
            -- v Notify subscribed clients
            subbedClids :: Array (Id "Client") <- Ref.read subsRef <#> Relation.rget cid >>> Set.toUnfoldable
            for_ subbedClids \subbedClid -> do
              clients <- Ref.read clientsRef
              let (subbed :: Client) = unsafePartial $ fromJust $ Map.lookup subbedClid clients
              subbed # Ws.transmit (List.singleton event)

    client # Ws.onClose do
      subsRef # Ref.modify_ (Relation.lexp clid)
      Console.log $ "End WebSocket connection: " <> unwrap clid
