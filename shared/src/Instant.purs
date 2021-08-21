module Y.Shared.Instant
  ( Instant
  , getNow
  , fromMilliseconds
  , asMilliseconds
  ) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Util.MonadJuggle (class MonadJuggle, juggle)
import Y.Shared.Pg.ToPg (class ToPg) as Pg
import Y.Shared.Pg.FromPg (class FromPg, mkImpl) as Pg
import Y.Shared.Pg.Types (PgExpr(..)) as Pg

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

instance toPg_Instant :: Pg.ToPg Instant where
  toPg = asMilliseconds >>> millisecondsToPgTimestamptz >>> Pg.PgExpr

instance fromPg_Instant :: Pg.FromPg Instant where
  impl = Pg.mkImpl $ pgTimestamptzToMilliseconds >>> map fromMilliseconds

pgTimestamptzToMilliseconds :: forall m. MonadJuggle String m => String -> m Number
pgTimestamptzToMilliseconds =
  pgTimestamptzToMilliseconds_f Nothing Just
  >>> case _ of
    Nothing -> juggle "Invalid date format"
    Just ms -> pure ms

foreign import pgTimestamptzToMilliseconds_f ::
  Maybe Number -> (Number -> Maybe Number) ->
  String -> Maybe Number

foreign import millisecondsToPgTimestamptz :: Number -> String
