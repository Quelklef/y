module Y.Client.Arrange where

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))

import Y.Client.ArrangementAlgorithms.Types (ArrangementAlgorithm(..))
import Y.Client.ArrangementAlgorithms.TreelikeNaive (arrange) as TreelikeNaive

algorithms :: Map String ArrangementAlgorithm
algorithms = Map.fromFoldable
  [ "treelike-naive" /\ ArrangementAlgorithm TreelikeNaive.arrange
  ]

defaultAlgoKey :: String
defaultAlgoKey = "treelike-naive"
