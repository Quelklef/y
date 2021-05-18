module Y.Shared.Id (Id, newId, parseId, isId) where

import Prelude

import Effect (Effect)

import Data.Newtype (class Newtype, unwrap)
import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.String.Common (toLower)

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

_tagName :: forall for. IsSymbol for => SProxy for -> String
_tagName proxy = reflectSymbol proxy # toLower

newId :: forall for. IsSymbol for => Effect (Id for)
newId = newId_f (_tagName (SProxy :: SProxy for))

foreign import newId_f :: forall for. String -> Effect (Id for)

-- | Parse an identifier
parseId :: forall for. IsSymbol for => String -> Maybe (Id for)
parseId id = parseId_f Nothing Just (_tagName (SProxy :: SProxy for)) id

foreign import parseId_f :: forall for.
  (forall a. Maybe a) -> (forall a. a -> Maybe a) ->
  String -> String -> Maybe (Id for)

-- | @isId s@ is the same as @isJust (parseId s)@
isId :: forall for. IsSymbol for => String -> Boolean
isId = map isJust (parseId :: String -> Maybe (Id for))
