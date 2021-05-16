module Shared.Instant where

import Data.Ord (class Ord)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic.Rep (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic.Rep (genericDecodeJson) as Agt

newtype Instant = Instant { milliseconds :: Int }

derive instance eqInstant :: Eq Instant
derive instance ordInstant :: Ord Instant

derive instance genericInstant :: Generic Instant _

instance encodeJsonInstant :: Agt.EncodeJson Instant where encodeJson = Agt.genericEncodeJson
instance decodeJsonInstant :: Agt.DecodeJson Instant where decodeJson = Agt.genericDecodeJson
