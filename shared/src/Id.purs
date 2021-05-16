module Shared.Id (Id, newId, parseId, isId) where

import Prelude

import Effect (Effect)

import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..), isJust)
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as Agt

newtype Id (for :: Symbol) = Id String

derive instance newtypeId :: Newtype (Id for) _
derive instance eqId :: Eq (Id for)
derive instance ordId :: Ord (Id for)

instance encodeJsonId :: Agt.EncodeJson (Id for) where encodeJson = unwrap >>> Agt.encodeJson
instance decodeJsonId :: Agt.DecodeJson (Id for) where decodeJson = Agt.decodeJson >>> map Id

instance showId :: Show (Id for) where
  show = unwrap >>> show

-- TODO: not sure if this possible but it would be cool to have (newId { namespace } :: Id namespace)
--       perhaps possible by requiring that namespace is a Symbol instead of a String
newId :: forall for. { namespace :: String } -> Effect (Id for)
newId { namespace } = newId_f namespace

foreign import newId_f :: forall for. String -> Effect (Id for)

-- | Parse an identifier
-- | If @namespace@ is non-@Nothing@, also validates the namespace.
parseId :: forall for. { namespace :: Maybe String } -> String -> Maybe (Id for)
parseId { namespace } id = parseId_f Nothing Just caseMaybeOf namespace id
  where
    caseMaybeOf :: forall a r. Maybe a -> r -> (a -> r) -> r
    caseMaybeOf maybe onNothing onJust = case maybe of
      Nothing -> onNothing
      Just x -> onJust x

foreign import parseId_f
  :: forall for.
     (forall a. Maybe a) -> (forall a. a -> Maybe a)  -- constructors for Maybe
  -> (forall a r. Maybe a -> r -> (a -> r) -> r)      -- destructor for Maybe
  -> Maybe String -> String -> Maybe (Id for)

-- | @isId n s@ is the same as @isJust (parseId n s)@
isId :: { namespace :: Maybe String } -> String -> Boolean
isId = (map >>> map) isJust parseId
