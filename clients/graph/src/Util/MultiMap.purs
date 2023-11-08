module Y.Client.Util.MultiMap
  ( MultiMap
  , singleton
  , insert
  , lookup
  , map
  , collapse
  ) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Functor as Functor

newtype MultiMap k v = MMap (Map k (Set v))

instance semigroupMultiMap :: (Ord k, Ord v) => Semigroup (MultiMap k v) where
  append (MMap mm1) (MMap mm2) = MMap $ Map.unionWith (<>) mm1 mm2

instance monoidMultiMap :: (Ord k, Ord v) => Monoid (MultiMap k v) where
  mempty = MMap Map.empty

singleton :: forall k v. k -> v -> MultiMap k v
singleton k v = MMap (Map.singleton k (Set.singleton v))

insert :: forall k v. Ord k => Ord v => k -> v -> MultiMap k v -> MultiMap k v
insert k v mmap = mmap <> singleton k v

lookup :: forall k v. Ord k => Ord v => k -> MultiMap k v -> Set v
lookup k (MMap mmap) = Map.lookup k mmap # fromMaybe mempty

map :: forall k v v'. Ord v' => (v -> v') -> MultiMap k v -> MultiMap k v'
map f (MMap mmap) = MMap $ Functor.map (Set.map f) mmap

collapse :: forall k v r. (Set v -> r) -> MultiMap k v -> Map k r
collapse f (MMap mmap) = mmap # Functor.map f
