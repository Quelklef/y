module Y.Client.ArrangementAlgorithms.TreelikeNaive (arrange) where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Foldable (sum, length, foldl)
import Partial.Unsafe (unsafePartial)

import Y.Client.Util.Vec2 (Vec2(..), getX, getY)

spacingX :: Number
spacingX = 50.0

spacingY :: Number
spacingY = 50.0

data Placements id = Placements
  { places :: Map id Vec2
  , left :: Number
  , right :: Number
  , top :: Number
  , bottom :: Number
  }

instance semigroupPlacements :: Ord id => Semigroup (Placements id) where
  append (Placements p1) (Placements p2) = Placements
    { places: Map.union p1.places p2.places  -- TODO
    , left: min p1.left p2.left
    , right: max p1.right p2.right
    , top: min p1.top p2.top
    , bottom: max p1.bottom p2.bottom
    }

instance monoidPlacements :: Ord id => Monoid (Placements id) where
  mempty = Placements
    { places: Map.empty
    , left: zero
    , right: zero
    , top: zero
    , bottom: zero
    }

shift :: forall id. Number -> Number -> Placements id -> Placements id
shift dx dy (Placements p) = Placements
  { places: p.places # map (_ + Vec2 { x: dx, y: dy })
  , left: p.left + dx
  , right: p.right + dx
  , top: p.top + dy
  , bottom: p.bottom + dy
  }

centerX :: forall id. Placements id -> Number
centerX p@(Placements { left, right }) = (left + right) / 2.0

setCenterX :: forall id. Number -> Placements id -> Placements id
setCenterX goal p = p # shift (goal - centerX p) 0.0

setLeft :: forall id. Number -> Placements id -> Placements id
setLeft goal p@(Placements { left }) = p # shift (goal - left) 0.0

setBottom :: forall id. Number -> Placements id -> Placements id
setBottom goal p@(Placements { bottom }) = p # shift 0.0 (goal - bottom)

setTop :: forall id. Number -> Placements id -> Placements id
setTop goal p@(Placements { top }) = p # shift 0.0 (goal - top)

average :: List Vec2 -> Maybe Vec2
average vs = if List.null vs then Nothing
             else let sx = vs # map getX # sum
                      sy = vs # map getY # sum
                      n = length vs
                  in Just $ Vec2 { x: sx / n, y: sy / n }

arrange :: forall node time id. Ord node => Ord time => Ord id =>
           { getId :: node -> id } ->
           { getDeps :: node -> List id } ->
           { getTime :: node -> time } ->
           { getDims :: node -> { width :: Number, height :: Number } } ->
           List node ->
           Map id Vec2

arrange { getId } { getDeps: getDepIds } { getTime } { getDims } allNodes =

  allNodes # ( stage1'assign >>> stage2'adjust >>> stage3'unwrap )

  where

  getNode :: id -> node
  getNode id = unsafePartial $ Map.lookup id nodesById # fromJust
    where nodesById = allNodes # map (\node -> getId node /\ node) # Map.fromFoldable

  getReplies :: node -> List node
  getReplies = \node -> Map.lookup (getId node) repliesMap # fromMaybe List.Nil
    where repliesMap = allNodes
                     >>= (\node -> getDepIds node # map \depId -> Map.singleton depId (List.singleton node))
                     # foldl (Map.unionWith (<>)) Map.empty

  singleton :: node -> Placements id
  singleton node = Placements
        { places: Map.singleton (getId node) zero
        , top: (getDims node).height * -0.5
        , bottom: (getDims node).height * 0.5
        , left: (getDims node).width * -0.5
        , right: (getDims node).width * 0.5
        }

  union :: List (Placements id) -> Placements id
  union = List.reverse >>> impl
    where
      impl List.Nil = mempty
      impl (List.Cons p List.Nil) = p
      impl (List.Cons p ps) =
        let ps'@(Placements ps'_) = impl ps
        in ps' <> (p # setLeft (ps'_.right + spacingX)
                     # setTop ps'_.top)

  place :: node -> Placements id
  place node =
    case getReplies node of
      List.Nil -> singleton node
      replies ->
        let
          unioned = replies # List.sortBy (comparing getTime) # map place # union
          Placements unioned_ = unioned
          singletond = singleton node # setCenterX (centerX unioned) # setBottom (unioned_.top - spacingY)
        in
          unioned <> singletond
          

  rootNodes = allNodes # List.filter (getDepIds >>> List.null)

  stage1'assign :: List node -> Placements id
  stage1'assign nodes = rootNodes # map place # union

  stage2'adjust :: Placements id -> Placements id
  stage2'adjust placements@(Placements placements_) =
    let getPlacement id = unsafePartial $ Map.lookup id placements_.places # fromJust
        center = rootNodes # map getId # map getPlacement # average # fromMaybe zero
        adjusted = placements # shift (negate (getX center)) (negate (getY center))
    in adjusted

  stage3'unwrap :: Placements id -> Map id Vec2
  stage3'unwrap (Placements p_) = p_.places
