module Y.Shared.Event where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)
import Data.Set (Set)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)

-- An Event is "something that happening"
-- Events are only ever created, never destroyed
newtype Event = Event
  { id :: Id "Event"
  , time :: Instant
  , payload :: EventPayload
  , convoId :: Id "Convo"
  }

data EventPayload
  -- A user set their display name
  = EventPayload_SetName
    { name :: String
    , userId :: Id "User"
    }

  -- A message was sent
  | EventPayload_MessageSend
    { messageId :: Id "Message"
    , depIds :: Set (Id "Message")
    , timeSent :: Instant
    , content :: String
    , userId :: Id "User"
    }

  -- A message was edited
  | EventPayload_MessageEdit
    { messageId :: Id "Message"
    , content :: String
    , userId :: Id "User"
    }

  -- A message was deleted
  | EventPayload_MessageDelete
    { messageId :: Id "Message"
    , userId :: Id "User"
    }

  -- A message was marked as read or unread
  | EventPayload_SetReadState
    { messageId :: Id "Message"
    , isUnread :: Boolean
    , userId :: Id "User"
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
