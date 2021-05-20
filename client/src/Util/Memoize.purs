module Y.Client.Util.Memoize where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..))
import Data.Map as Map

memoize :: forall x r. Ord x => (x -> r) -> (x -> r)
memoize = memoizeBy identity

memoizeBy :: forall x k r. Ord k => (x -> k) -> (x -> r) -> (x -> r)
memoizeBy getKey func = unsafePerformEffect do
  cacheRef <- Ref.new Map.empty
  pure $ \arg -> unsafePerformEffect do
    let key = getKey arg
    cache <- Ref.read cacheRef
    res <- case Map.lookup key cache of
      Just res -> pure res
      Nothing -> do
        let res = func arg
        Ref.modify_ (Map.insert key res) cacheRef
        pure res
    pure res
