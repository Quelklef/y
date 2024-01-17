module Y.Client.ToSub
  ( MorallySub
  , morallySubToSub
  , websocketClientToSub
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)

import Data.Batched (Batched (Single))
import Sub (Sub, SingleSub (SingleSub), Canceler)
import Producer (producer)

import Y.Shared.Util.Codable (class Decodable)

import Y.Client.WebSocket (Client, onTransmission)

-- | Turn a WebSocket @Client@ into an Elmish @Sub@
-- | Note that the resulting @Sub@ is *not* cancellable
websocketClientToSub :: forall m ts tc. Decodable m tc => Client ts tc -> Sub (m tc)
websocketClientToSub = websocketClientToMorallySub >>> morallySubToSub

-- What a @Sub@ is, morally speaking
-- Essentially, a sub gets passed an @update :: model -> Effect Unit@.
-- It's expected to use this function to set up the subscription, for instance
-- by kicking of an timer which will invoke @update@ once every second.
-- Then, it's expected to produce a @canceler :: Effect Unit@.
type MorallySub a = ((a -> Effect Unit) -> Effect Canceler)

websocketClientToMorallySub :: forall m ts tc. Decodable m tc => Client ts tc -> MorallySub (m tc)
websocketClientToMorallySub client = \update -> (client # onTransmission update) $> canceler
  where (canceler :: Canceler) = pure unit

morallySubToSub :: MorallySub ~> Sub
morallySubToSub ms = Single $ SingleSub $ producer unAlwaysEq (AlwaysEq ms)


-- workaround for elmish producer API
-- this is fragile and only works because we never cancel subscriptions

newtype AlwaysEq a = AlwaysEq a

unAlwaysEq :: forall a. AlwaysEq a -> a
unAlwaysEq (AlwaysEq a) = a

instance Eq (AlwaysEq a) where
  eq _ _ = true
