module Y.Client.ArrangementAlgorithms.TreelikeNaiveRadial (arrange) where

import Prelude

import Data.List (List)
import Data.Map (Map)

import Y.Client.Util.Vec2 (Vec2(..))
import Y.Client.Util.Vec2 as Vec2
import Y.Client.ArrangementAlgorithms.TreelikeNaive (arrange) as TreelikeNaive

arrange :: forall node time id x. Ord node => Ord time => Ord id =>
           { getId :: node -> id
           , getDepIds :: node -> List id
           , getTime :: node -> time
           , getDims :: node -> { width :: Number, height :: Number }
           , nodes :: List node
           | x
           }
           -> Map id Vec2

arrange = TreelikeNaive.arrange >>> map (\v -> v * norm' v * neg_i)
  where
  neg_i = Vec2 { x: 0.0, y: -1.0 }
  norm' v = if Vec2.mag v == 0.0 then Vec2.origin else Vec2.norm v
