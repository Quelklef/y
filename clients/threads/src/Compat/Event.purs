module Compat.Event where

import Prelude

import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)

import Compat.Id (convertId)
import Compat.Message (Message)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Event as Event
import Y.Shared.Id (Id)
import Y.Shared.Instant (Instant)

-- An Event is "something that happening"
-- Events are only ever created, never destroyed
newtype Event = Event
  { id :: Id "Event"
  , time :: Instant
  , payload :: EventPayload
  }


toEvent :: Id "Room" -> Event -> Event.Event
toEvent roomId (Event { id, time, payload }) =
  Event.Event
    { id
    , time
    , payload: toEventPayload payload
    , roomId
    }

fromEvent :: Event.Event -> Event
fromEvent (Event.Event { id, time, payload, roomId }) =
  Event
    { id
    , time
    , payload: fromEventPayload (convertId roomId) payload
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

  -- A message was edited
  | EventPayload_MessageEdit
    { convoId :: Id "Convo"
    , messageId :: Id "Message"
    , authorId :: Id "User"
    , content :: String
    }

  -- A message was deleted
  | EventPayload_MessageDelete
    { convoId :: Id "Convo"
    , userId :: Id "User"
    , messageId :: Id "Message"
    }

  -- A message was marked as read or unread
  | EventPayload_SetReadState
    { convoId :: Id "Convo"
    , userId :: Id "User"
    , messageId :: Id "Message"
    , readState :: Boolean
    }

toEventPayload :: EventPayload -> Event.EventPayload
toEventPayload = case _ of
  EventPayload_SetName { userId, name } ->
    Event.EventPayload_SetName { userId, name }

  EventPayload_MessageSend
    { message: { authorId, id, depIds, timeSent, content } } ->
    Event.EventPayload_MessageSend
      { messageId: id
      , depIds
      , timeSent
      , content
      , userId: authorId
      }

  EventPayload_MessageEdit { messageId, authorId, content } ->
    Event.EventPayload_MessageEdit
      { messageId
      , content
      , userId: authorId
      }

  EventPayload_MessageDelete { userId, messageId } ->
    Event.EventPayload_MessageDelete { userId, messageId }

  EventPayload_SetReadState { userId, messageId, readState } ->
    Event.EventPayload_MessageSetIsUnread
      { messageId
      , isUnread: not readState
      , userId
      }

fromEventPayload :: Id "Convo" -> Event.EventPayload -> EventPayload
fromEventPayload convoId = case _ of
  Event.EventPayload_SetName { userId, name } ->
    EventPayload_SetName { convoId, userId, name }

  Event.EventPayload_MessageSend
    { messageId
    , depIds
    , timeSent
    , content
    , userId
    } ->
    EventPayload_MessageSend
      { convoId
      , message:
          { authorId: userId
          , convoId
          , deleted: false
          , content
          , id: messageId
          , depIds
          , timeSent
          }
      }

  Event.EventPayload_MessageEdit
    { messageId
    , content
    , userId: authorId
    } ->
    EventPayload_MessageEdit
      { convoId
      , messageId
      , authorId
      , content
      }

  Event.EventPayload_MessageDelete { messageId, userId } ->
    EventPayload_MessageDelete { convoId, messageId, userId }

  Event.EventPayload_MessageSetIsUnread
    { messageId
    , isUnread
    , userId
    } ->
    EventPayload_SetReadState
      { convoId
      , userId
      , messageId
      , readState: not isUnread
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
