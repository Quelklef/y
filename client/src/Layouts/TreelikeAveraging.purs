module Y.Client.Layouts.TreelikeAveraging where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Foldable (class Foldable, sum, length, foldl)
import Partial.Unsafe (unsafePartial)

import Y.Client.Util.Vec2 (Vec2(..), getX, getY)
import Y.Client.Util.MultiMap (MultiMap)
import Y.Client.Util.MultiMap as MultiMap
import Y.Client.Layouts.Core (Layout (Generic))

spacingX :: Number
spacingX = 50.0

spacingY :: Number
spacingY = 50.0

-- | A mapping from node ids to placements, and a bounding box around this mapping
-- | Note that we allow multiple placements per node.
-- | In essense, we treat incoming graph structure as a nondeterministic tree.
type BoundedPlacements id =
  { places :: MultiMap id Vec2
  , left :: Number
  , right :: Number
  , top :: Number
  , bottom :: Number
  }

bp_append :: forall id. Ord id => BoundedPlacements id -> BoundedPlacements id -> BoundedPlacements id
bp_append p1 p2 =
  { places: p1.places <> p2.places
  , left: min p1.left p2.left
  , right: max p1.right p2.right
  , top: min p1.top p2.top
  , bottom: max p1.bottom p2.bottom
  }

bp_mempty :: forall id. Ord id => BoundedPlacements id
bp_mempty =
  { places: mempty
  , left: zero
  , right: zero
  , top: zero
  , bottom: zero
  }

bp_shift :: forall id. Number -> Number -> BoundedPlacements id -> BoundedPlacements id
bp_shift dx dy p =
  { places: p.places # MultiMap.map (_ + Vec2 { x: dx, y: dy })
  , left: p.left + dx
  , right: p.right + dx
  , top: p.top + dy
  , bottom: p.bottom + dy
  }

bp_centerX :: forall id. BoundedPlacements id -> Number
bp_centerX p = (p.left + p.right) / 2.0

bp_setCenterX :: forall id. Number -> BoundedPlacements id -> BoundedPlacements id
bp_setCenterX goal p = p # bp_shift (goal - bp_centerX p) 0.0

bp_setLeft :: forall id. Number -> BoundedPlacements id -> BoundedPlacements id
bp_setLeft goal p = p # bp_shift (goal - p.left) 0.0

bp_setBottom :: forall id. Number -> BoundedPlacements id -> BoundedPlacements id
bp_setBottom goal p = p # bp_shift 0.0 (goal - p.bottom)

bp_setTop :: forall id. Number -> BoundedPlacements id -> BoundedPlacements id
bp_setTop goal p = p # bp_shift 0.0 (goal - p.top)

average :: forall f. Foldable f => f Vec2 -> Maybe Vec2
average fs = let vs = List.fromFoldable fs
             in if List.null vs then Nothing
             else let sx = vs # map getX # sum
                      sy = vs # map getY # sum
                      n = length vs
                  in Just $ Vec2 { x: sx / n, y: sy / n }


layout :: Layout
layout = Generic arrange

arrange :: forall node time id x. Ord node => Ord time => Ord id =>
           { getId :: node -> id
           , getDepIds :: node -> List id
           , getTime :: node -> time
           , getDims :: node -> { width :: Number, height :: Number }
           , nodes :: List node
           | x
           }
           -> Map id Vec2

arrange { getId, getDepIds, getTime, getDims, nodes: allNodes } =

  allNodes # ( stage1'assign >>> stage2'resolve >>> stage3'adjust )

  where

  getNode :: id -> node
  getNode id = unsafePartial $ Map.lookup id nodesById # fromJust
    where nodesById = allNodes # map (\node -> getId node /\ node) # Map.fromFoldable

  getReplies :: node -> List node
  getReplies = \node -> Map.lookup (getId node) repliesMap # fromMaybe List.Nil
    where repliesMap = allNodes
                     >>= (\node -> getDepIds node # map \depId -> Map.singleton depId (List.singleton node))
                     # foldl (Map.unionWith (<>)) Map.empty

  bp_singleton :: node -> BoundedPlacements id
  bp_singleton node =
        let dims = getDims node in
        { places: MultiMap.singleton (getId node) zero
        , top: dims.height * -0.5
        , bottom: dims.height * 0.5
        , left: dims.width * -0.5
        , right: dims.width * 0.5
        }

  bp_concat :: List (BoundedPlacements id) -> BoundedPlacements id
  bp_concat = List.reverse >>> impl
    where
      impl List.Nil = bp_mempty
      impl (List.Cons p List.Nil) = p
      impl (List.Cons p ps) =
        let ps' = impl ps
            p' = p # bp_setLeft (ps'.right + spacingX)
                   # bp_setTop ps'.top
        in bp_append ps' p'

  place :: node -> BoundedPlacements id
  place node =
    case getReplies node of
      List.Nil -> bp_singleton node
      replies ->
        let catted = replies # List.sortBy (comparing getTime) # map place # bp_concat
            single = bp_singleton node # bp_setCenterX (bp_centerX catted) # bp_setBottom (catted.top - spacingY)
        in
          bp_append catted single
         

  rootNodes = allNodes # List.filter (getDepIds >>> List.null)

  stage1'assign :: List node -> BoundedPlacements id
  stage1'assign nodes = rootNodes # map place # bp_concat

  -- | Resolve placement multiplicity by averaging
  stage2'resolve :: BoundedPlacements id -> Map id Vec2
  stage2'resolve bp = bp.places # MultiMap.collapse (\placements -> unsafePartial $ fromJust $ average placements)

  stage3'adjust :: Map id Vec2 -> Map id Vec2
  stage3'adjust places = places # map (_ - center)
    where center = rootNodes # map getId # map getPlacement # average # fromMaybe zero
          getPlacement id = unsafePartial $ fromJust $ Map.lookup id places

