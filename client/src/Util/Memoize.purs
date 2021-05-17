module Client.Util.Memoize where

import Prelude

import Effect.Unsafe (unsafePerformEffect)
import Effect.Ref as Ref
import Data.Maybe (Maybe(..))
import Data.Map as Map

memoize :: forall v r. Ord v => (v -> r) -> (v -> r)
memoize f = unsafePerformEffect do
  cacheRef <- Ref.new Map.empty
  pure $ \v -> unsafePerformEffect do
    cache <- Ref.read cacheRef
    r <- case Map.lookup v cache of
      Nothing -> do
        let r = f v
        Ref.modify_ (Map.insert v r) cacheRef
        pure r
      Just r -> pure r
    pure r
