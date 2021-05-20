module Y.Shared.Id (Id, new, format, parse, isId) where

import Prelude

import Effect (Effect)

import Data.Maybe (Maybe(..), isJust)
import Data.Symbol (class IsSymbol, reflectSymbol, SProxy(..))
import Data.String.Common (toLower)

import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as Agt

newtype Id (for :: Symbol) = Id String

derive instance eqId :: Eq (Id for)
derive instance ordId :: Ord (Id for)

instance encodeJsonId :: Agt.EncodeJson (Id for) where encodeJson = format >>> Agt.encodeJson
instance decodeJsonId :: Agt.DecodeJson (Id for) where decodeJson = Agt.decodeJson >>> map Id

_tagName :: forall for. IsSymbol for => SProxy for -> String
_tagName proxy = reflectSymbol proxy # toLower

new :: forall for. IsSymbol for => Effect (Id for)
new = new_f (_tagName (SProxy :: SProxy for))

foreign import new_f :: forall for. String -> Effect (Id for)

format :: forall for. Id for -> String
format (Id s) = s

-- | Parse an identifier
parse :: forall for. IsSymbol for => String -> Maybe (Id for)
parse id = parse_f Nothing Just (_tagName (SProxy :: SProxy for)) id

foreign import parse_f :: forall for.
  (forall a. Maybe a) -> (forall a. a -> Maybe a) ->
  String -> String -> Maybe (Id for)

-- | @isId s@ is the same as @isJust (parse s)@
isId :: forall for. IsSymbol for => String -> Boolean
isId = map isJust (parse :: String -> Maybe (Id for))
