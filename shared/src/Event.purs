module Y.Shared.Event where

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
type Event =
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

derive instance genericEvent :: Generic EventPayload _

instance encodeJsonEvent :: Agt.EncodeJson EventPayload where encodeJson = Agt.genericEncodeJson
instance decodeJsonEvent :: Agt.DecodeJson EventPayload where decodeJson = Agt.genericDecodeJson
