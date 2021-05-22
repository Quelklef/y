module Y.Client.Arrange where

import Prelude

import Data.List (List)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))

import Y.Client.Util.Vec2 (Vec2)

import Y.Client.ArrangementAlgorithms.Bad as Bad
import Y.Client.ArrangementAlgorithms.TreelikeNaive as TreelikeNaive

newtype ArrangementAlgorithm = ArrangementAlgorithm (
    forall node time id. Ord node => Ord time => Ord id =>
    { getId :: node -> id } ->
    { getDeps :: node -> List id } ->
    { getTime :: node -> time } ->
    { getDims :: node -> { width :: Number, height :: Number } } ->
    List node ->
    Map id Vec2 )

algorithms :: Map String ArrangementAlgorithm
algorithms = Map.fromFoldable
  [ "bad" /\ ArrangementAlgorithm Bad.arrange
  , "treelike-naive" /\ ArrangementAlgorithm TreelikeNaive.arrange
  ]

defaultAlgoKey :: String
defaultAlgoKey = "treelike-naive"
