module Y.Shared.Util.Sorted where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Functor as Functor

-- n.b. This module could be greatly improved, but it works for now

class Functor l <= Sortable l where
  s_insert :: forall a. Ord a => a -> l a -> l a
  s_sort :: forall a. Ord a => l a -> l a
  s_empty :: forall a. l a

instance sortable_array :: Sortable Array where
  s_insert = Array.insert
  s_sort = Array.sort
  s_empty = mempty

instance sortable_list :: Sortable List where
  s_insert = List.insert
  s_sort = List.sort
  s_empty = mempty

newtype Sorted :: forall k. (k -> Type) -> k -> Type
newtype Sorted l a = Sorted (l a)

instance foldableSorted :: Foldable l => Foldable (Sorted l) where
  foldl f z (Sorted l) = foldl f z l
  foldr f z (Sorted l) = foldr f z l
  foldMap f (Sorted l) = foldMap f l

unSorted :: forall l a. Sorted l a -> l a
unSorted (Sorted l) = l

sort :: forall l a. Sortable l => Ord a => l a -> Sorted l a
sort l = Sorted (s_sort l)

insert :: forall l a. Sortable l => Ord a => a -> Sorted l a -> Sorted l a
insert a (Sorted l) = Sorted (s_insert a l)

map :: forall l a b. Sortable l => Ord b => (a -> b) -> Sorted l a -> Sorted l b
map f (Sorted l) = sort (Functor.map f l)

fromAsc :: forall l a. Sortable l => Ord a => l a -> Sorted l a
fromAsc = Sorted

empty :: forall l a. Sortable l => Ord a => Sorted l a
empty = Sorted s_empty
