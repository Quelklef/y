module Server.WebSocket where

import Prelude

import Effect (Effect)

import Shared.Codable (class Encodable, class Decodable, encode, decode)

-- | @Server ts tc@ is the type of WebSocket servers who receive transmissions
-- | of type @ts@ ("to server") and send transmissions of type @tc@ ("to client")
foreign import data Server :: Type -> Type -> Type
foreign import data Client :: Type -> Type -> Type

-- | Create a new server
foreign import newServer :: forall ts tc. { port :: Int } -> Effect (Server ts tc)

-- | Listen for new client connections
foreign import onConnection :: forall ts tc. (Client ts tc -> Effect Unit) -> Server ts tc -> Effect Unit

-- | Listen for client -> server transmissions
onTransmission :: forall m ts tc. Decodable m ts => (m ts -> Effect Unit) -> Client ts tc -> Effect Unit
onTransmission = onTransmission_f decode

foreign import onTransmission_f :: forall m ts tc.
  (String -> m ts) ->
  (m ts -> Effect Unit) -> Client ts tc -> Effect Unit

-- | Listen for client closes
foreign import onClose :: forall ts tc. Effect Unit -> Client ts tc -> Effect Unit

-- | Send a transmission to a client
transmit :: forall ts tc. Encodable tc => tc -> Client ts tc -> Effect Unit
transmit tn client = transmit_f (encode tn) client

foreign import transmit_f :: forall ts tc. String -> Client ts tc -> Effect Unit
