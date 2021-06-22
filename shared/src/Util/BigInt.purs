module Y.Shared.Util.BigInt
  ( BigInt
  , toNumber
  , fromNumber
  , toInt
  , fromInt
  , pow
  ) where

import Data.Maybe (Maybe(..))
import Data.Semiring (class Semiring)
import Data.Ring (class Ring)
import Data.CommutativeRing (class CommutativeRing)
import Data.EuclideanRing (class EuclideanRing)
import Data.Eq (class Eq)
import Data.Ord (class Ord, Ordering(..))

foreign import data BigInt :: Type

foreign import add_f :: BigInt -> BigInt -> BigInt
foreign import zero_f :: BigInt
foreign import mul_f :: BigInt -> BigInt -> BigInt
foreign import one_f :: BigInt

instance semiringBigInt :: Semiring BigInt where
  add = add_f
  zero = zero_f
  mul = mul_f
  one = one_f

foreign import sub_f :: BigInt -> BigInt -> BigInt

instance ringBigInt :: Ring BigInt where
  sub = sub_f

instance commutativeRingBigInt :: CommutativeRing BigInt

foreign import degree_f :: BigInt -> Int
foreign import div_f :: BigInt -> BigInt -> BigInt
foreign import mod_f :: BigInt -> BigInt -> BigInt

instance euclideanRingBigInt :: EuclideanRing BigInt where
  degree = degree_f
  div = div_f
  mod = mod_f

foreign import eq_f :: BigInt -> BigInt -> Boolean

instance eqBigInt :: Eq BigInt where
  eq = eq_f

foreign import compare_f ::
  Ordering -> Ordering -> Ordering ->
  BigInt -> BigInt -> Ordering

instance ordBigInt :: Ord BigInt where
  compare = compare_f LT EQ GT

foreign import toNumber :: BigInt -> Number

foreign import fromNumber_f ::
  (forall a. Maybe a) -> (forall a. a -> Maybe a) ->
  Number -> Maybe BigInt

fromNumber :: Number -> Maybe BigInt
fromNumber = fromNumber_f Nothing Just

foreign import toInt :: BigInt -> Int

foreign import fromInt :: Int -> BigInt

foreign import pow :: BigInt -> BigInt -> BigInt
