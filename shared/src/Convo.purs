module Y.Shared.Convo where

import Prelude

import Data.List (List)
import Data.Foldable (class Foldable, foldl)
import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.Generic.Rep (class Generic)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)

type Convo =
  { id :: Id "Convo"
  , events :: List Event  -- expected to be sorted by time
  }

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

-- A sequence of events gets folded into a single State
type ConvoState =
  { userNames :: Map (Id "User") String
  , messages :: Set Message
  }

type Message =
  { id :: Id "Message"
  , timeSent :: Instant
  , authorId :: Id "User"
  , convoId :: Id "Convo"
  , deps :: Set (Id "Message")
  , content :: String
  }

applyEvent :: Event -> (ConvoState -> ConvoState)
applyEvent event state = case event.payload of
  EventPayload_SetName pl -> state { userNames = state.userNames `Map.union` Map.singleton pl.userId pl.name }
  EventPayload_MessageSend pl -> state { messages = state.messages <> Set.singleton pl.message }

simulate :: forall f. Foldable f => f Event -> ConvoState
simulate = foldl (flip applyEvent) state0
  where state0 = { userNames: Map.empty, messages: Set.empty }
