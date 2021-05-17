module Shared.Util.Codable where

import Prelude

import Data.Maybe (Maybe)
import Data.Either (Either, hush)
import Data.Bifunctor (lmap)

import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Parser (jsonParser)

class Encodable a where
  encode :: a -> String

class Decodable m a where
  decode :: String -> m a

-- Expects that:
-- 1) @encode@ is an injection
-- 2) If @m@ is @Applicative@, then @encode >>> decode = pure@
class (Encodable a, Decodable m a) <= Codable m a

instance encodableArgonaut :: EncodeJson a => Encodable a where
  encode = encodeJson >>> stringify

instance decodableEitherArgonaut :: DecodeJson a => Decodable (Either String) a where
  decode str = do
    let contextualize err = "When attempting to decode: " <> str <> "\n" <> err
    json <- jsonParser str # lmap contextualize
    val <- decodeJson json # lmap (printJsonDecodeError >>> contextualize)
    pure val

instance decodableMaybeArgonaut :: DecodeJson a => Decodable Maybe a where
  decode = jsonParser >>> hush >=> (decodeJson >>> hush)
