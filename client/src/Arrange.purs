module Y.Client.Arrange where

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))

import Y.Client.ArrangementAlgorithms.Types (ArrangementAlgorithm(..))
import Y.Client.ArrangementAlgorithms.TreelikeNaive (arrange) as TreelikeNaive
import Y.Client.ArrangementAlgorithms.TreelikeNaiveRadial (arrange) as TreelikeNaiveRadial

algorithms :: Map String ArrangementAlgorithm
algorithms = Map.fromFoldable
  [ "treelike-naive" /\ ArrangementAlgorithm TreelikeNaive.arrange
  , "treelike-naive-radial" /\ ArrangementAlgorithm TreelikeNaiveRadial.arrange
  ]

defaultAlgoKey :: String
defaultAlgoKey = "treelike-naive"
