module Y.Client.Util.Vec2 where

import Prelude

import Data.Newtype (class Newtype, unwrap)
import Data.Semiring (class Semiring)
import Data.Ring (class Ring)
import Math as Math

newtype Vec2 = Vec2 { x :: Number, y :: Number }

derive instance newtypeVec2 :: Newtype Vec2 _

instance semiringVec2 :: Semiring Vec2 where
  add (Vec2 a) (Vec2 b) = Vec2 { x: a.x + b.x, y: a.y + b.y }
  zero = Vec2 { x: 0.0, y: 0.0 }
  mul (Vec2 a) (Vec2 b) = Vec2 { x: a.x * b.x - a.y * b.y, y: a.x * b.y + a.y * b.x }  -- complex multiplication
  one = Vec2 { x: 1.0, y: 0.0 }

instance ringVec2 :: Ring Vec2 where
  sub (Vec2 a) (Vec2 b) = Vec2 { x: a.x - b.x, y: a.y - b.y }

origin :: Vec2
origin = Vec2 { x: 0.0, y: 0.0 }

getX :: Vec2 -> Number
getX = unwrap >>> _.x

getY :: Vec2 -> Number
getY = unwrap >>> _.y

mag :: Vec2 -> Number
mag (Vec2 v) = Math.sqrt(Math.pow v.x 2.0 + Math.pow v.y 2.0)

angle :: Vec2 ->  Number
angle (Vec2 v) = Math.atan2 v.y v.x
