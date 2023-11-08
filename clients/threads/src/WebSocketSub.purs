module WebSocketSub (wsToSub) where

import MasonPrelude

import Data.Batched (Batched(..))
import Producer (producer2)
import RefEq (RefEq(..))
import Sub (CC, Sub, SingleSub(..), Canceler)
import Y.Client.WebSocket (Client, onTransmission)
import Y.Shared.Transmission (ToClient)

canceler :: Canceler
canceler = pure unit

wsToSub :: ∀ a b. (Maybe ToClient -> a) -> Client b ToClient -> Sub a
wsToSub toMsg client =
  Single
  $ SingleSub
  $ producer2 helper (RefEq toMsg) (RefEq client)

helper :: ∀ a b. RefEq (Maybe ToClient -> a) -> RefEq (Client b ToClient) -> CC a
helper (RefEq toMsg) (RefEq client) =
  \msgCallback -> do
     onTransmission
       (msgCallback <. toMsg)
       client

     pure canceler
