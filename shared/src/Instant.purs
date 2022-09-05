module Y.Shared.Instant
  ( Instant
  , getNow
  , fromSeconds
  , asSeconds
  , fromMilliseconds
  , asMilliseconds
  ) where

import Prelude

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Either (Either (..), note)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as Agt
import Data.Argonaut.Decode.Error (JsonDecodeError (..)) as Agt
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Gen (suchThat)
import Data.Show.Generic (genericShow)
import Data.String.Utils (startsWith, endsWith)
import Data.String.CodeUnits (slice)

import Database.Postgres.ToPg (class ToPg) as Pg
import Database.Postgres.FromPg (class FromPg, mkImpl) as Pg
import Database.Postgres.Types (PgExpr(..)) as Pg

newtype Instant = Instant { milliseconds :: Number }

getNow :: Effect Instant
getNow = getNow_f <#> \now -> Instant { milliseconds: now }

foreign import getNow_f :: Effect Number

derive instance eqInstant :: Eq Instant
derive instance ordInstant :: Ord Instant

derive instance genericInstant :: Generic Instant _
instance Arbitrary Instant where
  arbitrary = Instant <$> ( arbitrary `suchThat` (_ > zero) )
instance Show Instant where show = genericShow

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

pgTimestamptzToMilliseconds :: String -> Either String Number
pgTimestamptzToMilliseconds = pgTimestamptzToMilliseconds_f Nothing Just >>> note "Invalid date format"

foreign import pgTimestamptzToMilliseconds_f ::
  Maybe Number -> (Number -> Maybe Number) ->
  String -> Maybe Number

foreign import millisecondsToPgTimestamptz :: Number -> String


instance Agt.EncodeJson Instant where
  encodeJson = asMilliseconds >>> show >>> (\s -> "e+" <> s <> "ms") >>> Agt.encodeJson

instance Agt.DecodeJson Instant where
  decodeJson json = do
    str <- Agt.decodeJson json
    if not (startsWith "e+" str && endsWith "ms" str)
    then fail "bad format (prefix/suffix)"
    else
      let str' = slice 2 (-2) str
      in case Number.fromString str' of
        Nothing -> fail "bad format (number parse)"
        Just n -> pure $ fromMilliseconds n

    where

    fail = Left <<< Agt.TypeMismatch  -- no other function String -> JsonDecodeError =(
