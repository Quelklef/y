module Y.Client.Layout where

import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested ((/\))

import Y.Client.Layouts.Core (Layout)
import Y.Client.Layouts.Dagre (layout) as Dagre
import Y.Client.Layouts.TreelikeAveraging (layout) as TreelikeAveraging

layouts :: Map String Layout
layouts = Map.fromFoldable
  [ "dagre" /\ Dagre.layout
  , "treelike-averaging" /\ TreelikeAveraging.layout
  ]

defaultLayoutName :: String
defaultLayoutName = "dagre"
