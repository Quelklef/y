module Y.Client.Util.Sorted
  ( Sorted
  , unSorted
  , sort
  , insert
  , map
  ) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Functor as Functor

newtype Sorted a = Sorted (List a)

instance foldableSorted :: Foldable Sorted where
  foldl f z (Sorted l) = foldl f z l
  foldr f z (Sorted l) = foldr f z l
  foldMap f (Sorted l) = foldMap f l

unSorted :: forall a. Sorted a -> List a
unSorted (Sorted l) = l

sort :: forall a. Ord a => List a -> Sorted a
sort l = Sorted (List.sort l)

insert :: forall a. Ord a => a -> Sorted a -> Sorted a
insert a (Sorted l) = Sorted (List.insert a l)
-- ^ Apparently Data.List.insert handles sorting; awesome!

map :: forall a b. Ord b => (a -> b) -> Sorted a -> Sorted b
map f (Sorted l) = sort (Functor.map f l)
