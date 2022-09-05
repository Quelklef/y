module Y.Server.Test.Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (intercalate)
import Data.Unit (Unit)
import Data.Either (either)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Core (stringify) as Agt
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Generic.Rep (class Generic)

import Test.QuickCheck
import Test.QuickCheck.Arbitrary

import Y.Shared.Event (Event)


main :: Effect Unit
main = do

  log "Event serialization roundtrips"
  quickCheck' 1000 $ \(ev :: Event) -> let
    enc = encodeJson ev
    dec = decodeJson enc
    in dec == pure ev
       <?> (intercalate "\n\n"
              [ "evt: " <> show ev
              , "enc: " <> Agt.stringify enc
              , "dec: " <> either printJsonDecodeError show dec
              ])
