module Y.Shared.Event where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Message (Message)
import Y.Shared.Id (Id)

-- An Event is "something that happening"
-- Events are only ever created, never destroyed
newtype Event = Event
  { id :: Id "Event"
  , time :: Instant
  , payload :: EventPayload
  }

data EventPayload
  -- A user set their display name
  = EventPayload_SetName
    { convoId :: Id "Convo"
    , userId :: Id "User"
    , name :: String
    }

  -- A message was sent
  | EventPayload_MessageSend
    { convoId :: Id "Convo"
    , message :: Message
    }

derive instance eqEvent :: Eq Event
derive instance genericEvent :: Generic Event _

instance ordEvent :: Ord Event where
  compare (Event a) (Event b) =
    let key event = event.time /\ event.id
    in (comparing key) a b

instance encodeJsonEvent :: Agt.EncodeJson Event where encodeJson = Agt.genericEncodeJson
instance decodeJsonEvent :: Agt.DecodeJson Event where decodeJson = Agt.genericDecodeJson

derive instance eqEventPayload :: Eq EventPayload
derive instance genericEventPayload :: Generic EventPayload _

instance encodeJsonEventPayload :: Agt.EncodeJson EventPayload where encodeJson = Agt.genericEncodeJson
instance decodeJsonEventPayload :: Agt.DecodeJson EventPayload where decodeJson = Agt.genericDecodeJson
