module Y.Client.ToSub
  ( MorallySub
  , morallySubToSub
  , websocketClientToSub
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)

import Data.Batched (Batched(Single))
import Sub (Sub, SingleSub(SingleSub), Canceler, fromForeign)

import Y.Shared.Util.Codable (class Decodable)

import Y.Client.WebSocket (Client, onTransmission)
import Y.Client.TrivialProducer (trivialProducer)

-- | Turn a WebSocket @Client@ into an Elmish @Sub@
-- | Note that the resulting @Sub@ is *not* cancellable
websocketClientToSub :: forall m ts tc. Decodable m tc => Client ts tc -> Sub (m tc)
websocketClientToSub = websocketClientToMorallySub >>> morallySubToSub

-- What a @Sub@ is, morally speaking
-- Essentially, a sub gets passed an @update :: model -> Effect Unit@.
-- It's expected to use this function to set up the subscription, for instance
-- by kicking of an timer which will invoke @update@ once every second.
-- Then, it's expected to produce a @canceler :: Effect Unit@.
type MorallySub a = ((a -> Effect Unit) -> Effect (Canceler))

type ForeignCC a = EffectFn1 (EffectFn1 a Unit) Canceler

websocketClientToMorallySub :: forall m ts tc. Decodable m tc => Client ts tc -> MorallySub (m tc)
websocketClientToMorallySub client = \update -> (client # onTransmission update) $> canceler
  where (canceler :: Canceler) = pure unit

morallySubToSub :: MorallySub ~> Sub
morallySubToSub m = Single $ SingleSub $ trivialProducer $ fromForeign $ morallySubToForeignCC m
  where
  morallySubToForeignCC :: MorallySub ~> ForeignCC
  morallySubToForeignCC ms = mkEffectFn1 (\update -> ms (runEffectFn1 update))
