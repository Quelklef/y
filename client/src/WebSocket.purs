module Y.Client.WebSocket where

import Prelude

import Effect (Effect)

import Y.Shared.Util.Codable (class Encodable, class Decodable, encode, decode)

-- | @Client ts tc@ is the type of WebSocket clients who send transmissions
-- | of type @ts@ ("to server") and receive tranmissions of type @tc@ ("to client")
foreign import data Client :: Type -> Type -> Type

foreign import newConnection :: forall ts tc. { url :: String } -> Effect (Client ts tc)

foreign import onOpen :: forall ts tc. Effect Unit -> Client ts tc -> Effect Unit

onTransmission :: forall m ts tc. Decodable m tc => (m tc -> Effect Unit) -> Client ts tc -> Effect Unit
onTransmission = onTransmission_f decode

foreign import onTransmission_f :: forall m ts tc.
  (String -> m tc) ->
  (m tc -> Effect Unit) -> Client ts tc -> Effect Unit

transmit :: forall ts tc. Encodable ts => ts -> Client ts tc -> Effect Unit
transmit tn client = do
  transmit_f (encode tn) client

foreign import transmit_f :: forall ts tc. String -> Client ts tc -> Effect Unit
