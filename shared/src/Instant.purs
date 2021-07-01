module Y.Shared.Instant
  ( Instant
  , getNow
  , fromMilliseconds
  , asMilliseconds
  ) where

import Prelude

import Effect (Effect)
import Data.Int as Int
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.ToFromPostgres as TFPg

newtype Instant = Instant { milliseconds :: Number }

getNow :: Effect Instant
getNow = getNow_f <#> \now -> Instant { milliseconds: now }

foreign import getNow_f :: Effect Number

derive instance eqInstant :: Eq Instant
derive instance ordInstant :: Ord Instant

derive instance genericInstant :: Generic Instant _

instance encodeJsonInstant :: Agt.EncodeJson Instant where encodeJson = Agt.genericEncodeJson
instance decodeJsonInstant :: Agt.DecodeJson Instant where decodeJson = Agt.genericDecodeJson

fromMilliseconds :: Number -> Instant
fromMilliseconds ms = Instant { milliseconds: ms }

asMilliseconds :: Instant -> Number
asMilliseconds (Instant i) = i.milliseconds

fromSeconds :: Number -> Instant
fromSeconds = (_ * 1000.0) >>> fromMilliseconds

asSeconds :: Instant -> Number
asSeconds = asMilliseconds >>> (_ / 1000.0)

instance toPg_Instant :: TFPg.ToPg Instant where
  toPg = asMilliseconds >>> millisecondsToPgTimestamptz

instance fromPg_Instant :: TFPg.FromPg (Either String) Instant where
  fromPg = TFPg.fromPg >=> pgTimestamptzToMilliseconds >>> map fromMilliseconds

pgTimestamptzToMilliseconds :: String -> Either String Number
pgTimestamptzToMilliseconds =
  pgTimestamptzToMilliseconds_f Nothing Just
  >>> case _ of
    Nothing -> Left "Invalid date format"
    Just ms -> Right ms

foreign import pgTimestamptzToMilliseconds_f ::
  Maybe Number -> (Number -> Maybe Number) ->
  String -> Maybe Number

foreign import millisecondsToPgTimestamptz :: Number -> String
