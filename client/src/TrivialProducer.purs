module Y.Client.TrivialProducer (trivialProducer) where

import Prelude

import Producer (Producer, producer)

trivialProducer :: forall a. a -> Producer a
trivialProducer val = producer (const val) NeverEq

-- Please don't ask questions
data NeverEq = NeverEq
instance Eq NeverEq where
  eq _ _ = false
