module Y.Shared.Id (Id, new, format, parse) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Exception (throw)
import Type.Proxy (Proxy (Proxy))
import Data.Maybe (Maybe (..), fromJust, fromMaybe)
import Data.Either (Either (..), note)
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.String.Common (toLower, split)
import Data.String.CodeUnits (toCharArray)
import Data.String.CodeUnits (length, slice, singleton) as String
import Data.String.CodePoints (indexOf)
import Data.String.Pattern (Pattern (..))
import Data.Traversable (traverse)
import Data.Foldable (foldl)
import Partial.Unsafe (unsafePartial)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Test.QuickCheck.Arbitrary (class Arbitrary, genericArbitrary)

import Data.Argonaut.Encode (class EncodeJson, encodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson, decodeJson) as Agt

import Database.Postgres.ToPg (class ToPg, toPg) as Pg
import Database.Postgres.FromPg (class FromPg, mkImpl) as Pg

import Y.Shared.Util.BigInt (BigInt)
import Y.Shared.Util.BigInt as BigInt

-- | Characters used in IDs
-- | Order is significant
idCharSeq :: String
idCharSeq = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

-- | Characters allowed in namespaces
-- | Order is significant
nsAllowedCharSeq :: String
nsAllowedCharSeq = "abcdefghijklmnopqrstuvwxyz-_ "

fromDigits :: String -> String -> Maybe BigInt
fromDigits digits =
  toCharArray
  >>> traverse (\ch -> indexOf (Pattern $ String.singleton ch) digits)
  >>> map (map BigInt.fromInt)
  >>> map (foldl (\acc digit -> acc * base + digit) zero)
  where base = BigInt.fromInt $ String.length digits

toDigits :: String -> BigInt -> String
toDigits digits n =
  let low = String.singleton $ unsafePartial $ fromJust $ Array.index (toCharArray digits) (BigInt.toInt $ n `mod` base)
      rest = n `div` base
      high = if rest > zero then toDigits digits rest else ""
  in high <> low
  where base = BigInt.fromInt $ String.length digits

newtype AgtBigInt = AgtBigInt BigInt

derive instance genericAgtBigInt :: Generic AgtBigInt _

instance encodeJsonAgtBigInt :: Agt.EncodeJson AgtBigInt
  where encodeJson (AgtBigInt b) = Agt.encodeJson (BigInt.toNumber b)
instance decodeJsonAgtBigInt :: Agt.DecodeJson AgtBigInt
  where decodeJson = Agt.decodeJson >>> map (BigInt.fromNumber >>> fromMaybe zero >>> AgtBigInt)
                                              -- TODO: fail on Nothing ^^
newtype Id (namespace :: Symbol) = Id { time :: BigInt, rand :: BigInt }

derive instance Generic (Id ns) _
instance Arbitrary (Id ns) where arbitrary = genericArbitrary

derive instance eqId :: Eq (Id ns)
derive instance ordId :: Ord (Id ns)
instance Show (Id ns) where show = genericShow

instance encodeJsonId :: IsSymbol ns => Agt.EncodeJson (Id ns)
  where encodeJson (Id { time, rand }) = Agt.encodeJson (AgtBigInt time /\ AgtBigInt rand)
instance decodeJsonId :: IsSymbol ns => Agt.DecodeJson (Id ns)
  where decodeJson = Agt.decodeJson
                 >>> map (\(AgtBigInt time /\ AgtBigInt rand) -> Id { time, rand })
                 -- ^^ TODO: when decoding, fail on wrong namespace

foreign import getNow :: Effect BigInt
foreign import getRand :: BigInt -> Effect BigInt

maxRand :: BigInt
maxRand = BigInt.pow (BigInt.fromInt $ String.length idCharSeq) len - one
  where len = BigInt.fromInt 4

getNs :: forall ns. IsSymbol ns => Proxy ns -> Effect BigInt
getNs proxy =
  case reflectSymbol proxy of
    "" -> throw "Invalid namespace: cannot be empty"
    s -> case s # toLower # fromDigits nsAllowedCharSeq of
      Nothing -> throw "Invalid namespace: contains invalid char"
      Just n -> pure n

-- | Generate a new identifier
new :: forall ns. IsSymbol ns => Effect (Id ns)
new = do
  _ <- getNs (Proxy :: Proxy ns)
  time <- getNow
  rand <- getRand maxRand
  pure $ Id { time, rand }

-- | Convert an identifier to a string
format :: forall ns. IsSymbol ns => Id ns -> String
format (Id id) =
  let
    ns = toDigits idCharSeq $ unsafePerformEffect $ getNs (Proxy :: Proxy ns)
    ns0 = reflectSymbol (Proxy :: Proxy ns) # String.slice 0 1
    timeStr = toDigits idCharSeq id.time
    randStr = toDigits idCharSeq id.rand
  in
    ns0 <> "-" <> ns <> "-" <> timeStr <> "-" <> randStr

-- | Parse an identifier
parse :: forall ns. IsSymbol ns => String -> Either String (Id ns)
parse = split (Pattern "-") >>> case _ of
  [_ns0, ns, timeStr, randStr] -> do
    when (ns /= (toDigits idCharSeq $ unsafePerformEffect $ getNs (Proxy :: Proxy ns))) $ Left "Wrong namespace"
    time <- fromDigits idCharSeq timeStr # note "Invalid timestamp"
    rand <- fromDigits idCharSeq randStr # note "Invalid random"
    pure $ Id { time, rand }
  _ -> Left "Wrong number of parts"

instance toPg_Id :: IsSymbol ns => Pg.ToPg (Id ns) where
  toPg = format >>> Pg.toPg

instance fromPg_Id :: IsSymbol ns => Pg.FromPg (Id ns) where
  impl = Pg.mkImpl parse
