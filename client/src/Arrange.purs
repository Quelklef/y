module Y.Client.Arrange where

import Prelude

import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Foldable (sum, length, foldl, maximum, minimum)
import Data.Function (on)
import Data.Int as Int
import Partial.Unsafe (unsafePartial)

import Y.Client.Util.Vec2 (Vec2(..), getX, getY)

arrange :: forall node time id. Ord node => Ord time => Ord id =>
           { getId :: node -> id } ->
           { getDeps :: node -> List id } ->
           { getTime :: node -> time } ->
           { getDims :: node -> { width :: Number, height :: Number } } ->
           List node ->
           Map id Vec2

arrange { getId } { getDeps } { getTime } { getDims } nodes =
  nodes
  # List.sortBy (comparing getTime)
  # List.mapWithIndex (\idx node -> getId node /\ Vec2 { x: 0.0, y: Int.toNumber idx * 100.0 })
  # Map.fromFoldable
