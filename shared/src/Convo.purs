module Shared.Convo where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson) as Agt

import Shared.Id (Id)
import Shared.Instant (Instant)

type Convo =
  { id :: Id "Convo"
  , events :: List Event
  }

-- An Event is "something that happening"
-- Events are only ever created, never destroyed
data Event
  -- A user set their display name
  = Event_SetName
    { id :: Id "Event"
    , time :: Instant
    , userId :: Id "User"
    , convoId :: Id "Convo"
    , name :: String
    }

  -- A message was sent
  | Event_MessageSend
    { id :: Id "Event"
    , time :: Instant
    , messageId :: Id "Message"
    , authorId :: Id "User"
    , convoId :: Id "Convo"
    , deps :: List (Id "Message")
    , content :: String
    }

derive instance genericEvent :: Generic Event _

instance encodeJsonEvent :: Agt.EncodeJson Event where encodeJson = Agt.genericEncodeJson
instance decodeJsonEvent :: Agt.DecodeJson Event where decodeJson = Agt.genericDecodeJson

-- A sequence of events gets folded into a single State
type State =
  { userNames :: Map (Id "User") String
  , messages :: List Message
  }

type Message =
  { id :: Id "Message"
  , time :: Instant
  , deps :: List (Id "Message")
  , content :: String
  }

applyEvent :: Event -> (State -> State)
applyEvent event state = case event of
  Event_SetName ev -> state { userNames = state.userNames <> Map.singleton ev.userId ev.name }
  Event_MessageSend ev -> state { messages = state.messages <> List.singleton { id: ev.messageId, time: ev.time, deps: ev.deps, content: ev.content } }

simulate :: List Event -> State
simulate = foldl (flip applyEvent) state0
  where state0 = { userNames: Map.empty, messages: List.Nil }
