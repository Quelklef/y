module Y.Server.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Unit (Unit)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Generic.Rep (class Generic)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Y.Shared.Event (Event)


main :: Effect Unit
main = do

  log "Event serialization roundtrips"
  quickCheck' 1000 $ \(ev :: Event) -> (ev # encodeJson # decodeJson) === pure ev
