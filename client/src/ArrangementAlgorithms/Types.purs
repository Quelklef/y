module Y.Client.ArrangementAlgorithms.Types where

import Prelude

import Data.Map (Map)
import Data.List (List)

import Y.Client.Util.Vec2 (Vec2)

-- Not sure why this has to be a newtype... seems like otherwise constraints float or something
newtype ArrangementAlgorithm = ArrangementAlgorithm (
      forall node time id x. Ord node => Ord time => Ord id =>
    { getId :: node -> id
    , getDepIds :: node -> List id
    , getTime :: node -> time
    , getDims :: node -> { width :: Number, height :: Number }
    , nodes :: List node
    | x
    }
    -> Map id Vec2
)
