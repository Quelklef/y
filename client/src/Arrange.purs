module Client.Arrange where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Foldable (class Foldable, sum, length, foldl, maximum, minimum)
import Data.Function (on)
import Data.Int as Int
import Partial.Unsafe (unsafePartial)

import Client.Util.Vec2 (Vec2(..), getX, getY)

type GetId node id = node -> id
type GetDeps node id = node -> List id
type GetTime node time = node -> time
type GetDims node = node -> { width :: Number, height :: Number }

calcTiers :: forall node id. Ord node => Eq id =>
             GetId node id -> GetDeps node id ->
             List node -> Map node Int
calcTiers getId getDeps nodes =
  let roots = nodes # List.filter (getDeps >>> List.null)
  in go roots 0
  where
    go these tier =
      if List.null these then Map.empty else
      let entries = these # map (_ /\ tier) # Map.fromFoldable
          theseIds = getId <$> these
          those = nodes # List.filter (getDeps >>> List.intersect theseIds >>> List.length >>> (_ > 0))
      in entries `unionR` go those (tier + 1)
    
    unionR = flip Map.union  -- right-biased union

arrange :: forall node time id. Ord node => Ord time => Ord id =>
           GetId node id -> GetDeps node id -> GetTime node time -> GetDims node ->
           List node -> Map id Vec2
arrange getId getDeps getTime getDims =
          stage_1'formativeArrangement
      >>> stage_2'normalization
      >>> stage_3'jostling

  where

    average xs = if List.null xs then Nothing else Just $ sum xs / length xs

    -- unsafe variants. teehee

    lookup' :: forall k v. Ord k => k -> Map k v -> v
    lookup' k m = unsafePartial $ fromJust (Map.lookup k m)

    maximum' :: forall f a. Foldable f => Ord a => f a -> a
    maximum' xs = unsafePartial $ fromJust $ maximum xs

    minimum' :: forall f a. Foldable f => Ord a => f a -> a
    minimum' xs = unsafePartial $ fromJust $ minimum xs

    -- Stage 1. Formative arrangements
    --          Y-positions are assigned as a trivial function of tier
    --          X-positions are assigned as follows.
    --          For a given tier, first, calculate the ideal x-position of each node, defined to
    --          be the average of the x-positions of its dependencies.
    --          Then, order the nodes by (ideal x-position, chronology)
    --          Then, divide the x-axis among these nodes in proportion to their weight.
    --          Finally, place each node halfway between the center of its portion and
    --          the closest x-position in its portion to its ideal x-position.
    stage_1'formativeArrangement :: List node -> Map id Vec2
    stage_1'formativeArrangement allNodes = go 0 Map.empty
      where
        tierByNode = calcTiers getId getDeps allNodes
        nodesByTier = invertMap tierByNode
        allTiers = Map.keys nodesByTier # List.fromFoldable

        -- The weight of a node is the maximum number of nodes in any descendant tiers
        weight :: node -> Int
        weight node = allTiers
                    # List.filter (_ > (tierByNode # lookup' node))
                    # map (\tier -> nodesByTier # lookup' tier # length)
                    # maximum
                    # fromMaybe 0

        go tier soFar =
          case Map.lookup tier nodesByTier of
            Nothing -> soFar
            Just these -> go (tier + 1) (soFar `Map.union` arrangeTierBasedOnArrangementSoFar tier soFar these)

        arrangeTierBasedOnArrangementSoFar :: Int -> Map id Vec2 -> List node -> Map id Vec2
        arrangeTierBasedOnArrangementSoFar tier soFar nodes =
          let
            calcIdealX = \node -> node # getDeps # map (\dep -> lookup' dep soFar # getX) # average # fromMaybe 0.0
            nodesOrdered = nodes # List.sortBy (compare `on` \node -> calcIdealX node /\ getTime node)
            calcPortion = \node -> Int.toNumber (weight node) / Int.toNumber (sum $ map weight nodes)
            nodePlacements =
              ((nodesOrdered
              # List.mapWithIndex \idx node ->
                let minX = List.slice 0 idx nodesOrdered # map calcPortion # sum
                    maxX = minX + calcPortion node
                    boundedIdealX = clamp minX maxX (calcIdealX node)
                    centerX = minX + (calcPortion node) / 2.0
                    xPlacement = (centerX + boundedIdealX) / 2.0
                    yPlacement = Int.toNumber tier
                in getId node /\ Vec2 { x: xPlacement, y: yPlacement }) :: List (id /\ Vec2))
              # Map.fromFoldable
          in nodePlacements

    -- Stage 2. Normalization
    --          Stage 1 assigned x- and y- positions with concern only to relative values.
    --          Now we will fix this, by scaling the given values into a "reasonable" range.
    --          Note that the x- and y- axes need to be normalized independently.
    stage_2'normalization :: Map id Vec2 -> Map id Vec2
    stage_2'normalization placements =
      if Map.isEmpty placements then Map.empty else
      let
        xs = placements # map getX
        ys = placements # map getY
        xRange = maximum' xs - minimum' xs
        yRange = maximum' ys - minimum' ys
        nodeCount = placements # Map.keys # length
        targetXRange = Int.toNumber $ nodeCount * 300
        targetYRange = Int.toNumber $ nodeCount * 100
        xScale = targetXRange / xRange
        yScale = targetYRange / yRange
      in
        placements # map \(Vec2 { x, y }) -> Vec2 { x: x * xScale, y: y * yScale }

    -- Stage 3. Jostling
    --          Nudge around nodes that are too close to each other
    stage_3'jostling :: Map id Vec2 -> Map id Vec2
    stage_3'jostling placements = placements


invertMap :: forall k v. Ord k => Ord v => Map k v -> Map v (List k)
invertMap = (Map.toUnfoldable :: Map k v -> List (k /\ v))
     >>> map (\(k /\ v) -> Map.singleton v (List.singleton k))
     >>> foldl (Map.unionWith (<>)) Map.empty
