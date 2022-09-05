module Y.Shared.Event where

import Prelude

import Control.Alt ((<|>))
import Data.Either (Either, note)
import Data.Tuple.Nested ((/\))
import Data.Generic.Rep (class Generic)
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe (..))
import Data.Show.Generic (genericShow)
import Foreign.Object (Object, lookup)

import Data.Argonaut.Core (Json, toObject, toString, fromObject) as Agt
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as Agt
import Data.Argonaut.Decode.Error (JsonDecodeError (..)) as Agt
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary, genericArbitrary)

import Y.Shared.Instant (Instant)
import Y.Shared.Id (Id)

-- An Event is "something that happening"
-- Events are only ever created, never destroyed
newtype Event = Event
  { id :: Id "Event"
  , time :: Instant
  , payload :: EventPayload
  , roomId :: Id "Room"
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
  | EventPayload_MessageSetIsUnread
    { messageId :: Id "Message"
    , isUnread :: Boolean
    , userId :: Id "User"
    }

derive instance eqEvent :: Eq Event
derive instance genericEvent :: Generic Event _

instance Show Event where show = genericShow

instance ordEvent :: Ord Event where
  compare (Event a) (Event b) =
    let key event = event.time /\ event.id
    in (comparing key) a b

derive newtype instance Agt.EncodeJson Event
derive newtype instance Agt.DecodeJson Event
instance Arbitrary Event where arbitrary = genericArbitrary

derive instance eqEventPayload :: Eq EventPayload
derive instance genericEventPayload :: Generic EventPayload _

instance Show EventPayload where show = genericShow

instance Arbitrary EventPayload where
  arbitrary =
        ((\name userId -> EventPayload_SetName { name, userId })
            <$> arbitrary <*> arbitrary)
    <|> ((\messageId depIds timeSent content userId -> EventPayload_MessageSend { messageId, depIds, timeSent, content, userId })
            <$> arbitrary <*> (arrayToSet <$> arbitrary) <*> arbitrary <*> arbitrary <*> arbitrary)
    <|> ((\messageId content userId -> EventPayload_MessageEdit { messageId, content, userId })
            <$> arbitrary <*> arbitrary <*> arbitrary)
    <|> ((\messageId userId -> EventPayload_MessageDelete { messageId, userId })
            <$> arbitrary <*> arbitrary)
    <|> ((\messageId isUnread userId -> EventPayload_MessageSetIsUnread { messageId, isUnread, userId })
            <$> arbitrary <*> arbitrary <*> arbitrary)

      where

      arrayToSet :: forall a. Ord a => Array a -> Set a
      arrayToSet = Set.fromFoldable

instance Agt.EncodeJson EventPayload where
  encodeJson = case _ of
    EventPayload_SetName x -> withProp "kind" "SetName" x
    EventPayload_MessageSend x -> withProp "kind" "MessageSend" x
    EventPayload_MessageEdit x -> withProp "kind" "MessageEdit" x
    EventPayload_MessageDelete x -> withProp "kind" "MessageDelete" x
    EventPayload_MessageSetIsUnread x -> withProp "kind" "MessageSetIsUnread" x

    where

    withProp :: forall a v. Agt.EncodeJson a => Agt.EncodeJson v => String -> v -> a -> Agt.Json
    withProp k v a =
      case Agt.toObject (Agt.encodeJson a) of
        Nothing -> Agt.encodeJson a
        Just o -> Agt.fromObject (withProp_f k (Agt.encodeJson v) o)

foreign import withProp_f :: forall a. String -> a -> Object a -> Object a

instance Agt.DecodeJson EventPayload where
  decodeJson json =
    do
      let
        note' :: forall v. String -> Maybe v -> Either Agt.JsonDecodeError v
        note' s v = note (Agt.TypeMismatch s) v
      obj <- Agt.toObject json # note' "expected object"
      kindJson <- lookup "kind" obj # note' "expected kind key"
      kindStr <- Agt.toString kindJson # note' "expected kind is string"
      case kindStr of
        "SetName" -> EventPayload_SetName <$> Agt.decodeJson json
        "MessageSend" -> EventPayload_MessageSend <$> Agt.decodeJson json
        "MessageEdit" -> EventPayload_MessageEdit <$> Agt.decodeJson json
        "MessageDelete" -> EventPayload_MessageDelete <$> Agt.decodeJson json
        "MessageSetIsUnread" -> EventPayload_MessageSetIsUnread <$> Agt.decodeJson json
        _ -> note' "bad kind" Nothing

