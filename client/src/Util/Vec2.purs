module Client.Util.Vec2 where

import Prelude

import Data.Newtype (class Newtype, unwrap)

newtype Vec2 = Vec2 { x :: Number, y :: Number }

derive instance newtypeVec2 :: Newtype Vec2 _

origin :: Vec2
origin = Vec2 { x: 0.0, y: 0.0 }

getX :: Vec2 -> Number
getX = unwrap >>> _.x

getY :: Vec2 -> Number
getY = unwrap >>> _.y
