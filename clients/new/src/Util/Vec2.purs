module Y.Client.Util.Vec2 where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Number as Math

newtype Vec2 = Vec2 { x :: Number, y :: Number }

derive instance newtypeVec2 :: Newtype Vec2 _

derive instance eqVec2 :: Eq Vec2
derive instance ordVec2 :: Ord Vec2

instance semiringVec2 :: Semiring Vec2 where
  add (Vec2 a) (Vec2 b) = Vec2 { x: a.x + b.x, y: a.y + b.y }
  zero = Vec2 { x: 0.0, y: 0.0 }
  mul (Vec2 a) (Vec2 b) = Vec2 { x: a.x * b.x - a.y * b.y, y: a.x * b.y + a.y * b.x }  -- complex multiplication
  one = Vec2 { x: 1.0, y: 0.0 }

instance ringVec2 :: Ring Vec2 where
  sub (Vec2 a) (Vec2 b) = Vec2 { x: a.x - b.x, y: a.y - b.y }

instance semigroupVec2 :: Semigroup Vec2 where
  append = add

instance monoidVec2 :: Monoid Vec2 where
  mempty = zero

origin :: Vec2
origin = Vec2 { x: 0.0, y: 0.0 }

getX :: Vec2 -> Number
getX = unwrap >>> _.x

getY :: Vec2 -> Number
getY = unwrap >>> _.y

mag :: Vec2 -> Number
mag (Vec2 v) = Math.sqrt (Math.pow v.x 2.0 + Math.pow v.y 2.0)

angle :: Vec2 ->  Number
angle (Vec2 v) = Math.atan2 v.y v.x

scale :: Number -> Vec2 -> Vec2
scale c (Vec2 v) = Vec2 { x: c * v.x, y: c * v.y }

norm :: Vec2 -> Vec2
norm v = scale (1.0 / mag v) v
