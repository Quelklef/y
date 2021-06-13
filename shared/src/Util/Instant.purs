module Y.Shared.Util.Instant
  ( Instant
  , getNow
  , fromMilliseconds
  , asMilliseconds
  ) where

import Prelude

import Effect (Effect)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

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
