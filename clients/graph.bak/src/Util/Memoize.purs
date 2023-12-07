module Y.Client.Util.Memoize where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map

memoize :: forall x k r. Ord k =>
  { func :: x -> r
  , by :: x -> k
  , with :: Ref (Map k r)
  }
  -> (x -> r)
memoize { func, by: getKey, with: cacheRef } = \arg -> unsafePerformEffect do
  let key = getKey arg
  cache <- Ref.read cacheRef
  res <- case Map.lookup key cache of
    Just res -> pure res
    Nothing -> do
      let res = func arg
      Ref.modify_ (Map.insert key res) cacheRef
      pure res
  pure res
