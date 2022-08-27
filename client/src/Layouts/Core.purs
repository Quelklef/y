module Y.Client.Layouts.Core where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Tuple.Nested ((/\))
import Foreign.Object (Object)
import Foreign.Object as Obj
import Data.Nullable (Nullable, toNullable)

import Y.Client.Util.Vec2 (Vec2 (..))
import Y.Shared.Instant (Instant)


data Layout

    -- | Use this variant to show off how generic your algorithm is
    = Generic (
        forall node time id x. Ord node => Ord time => Ord id =>
        { getId :: node -> id
        , getDepIds :: node -> List id
        , getTime :: node -> time
        , getDims :: node -> { width :: Number, height :: Number }
        , nodes :: List node
        , screenDims :: { width :: Number, height :: Number }
        , focusedId :: Maybe id
        | x
        }
        -> Map id Vec2
    )

    -- | Use this variant for foreign implementations
    | Foreign (
        forall x.
        { nodes :: Object FNode  -- maps id to node
        , screenDims :: { width :: Number, height :: Number }
        , focused :: Nullable FNode
        | x
        }
        -> Object { x :: Number, y :: Number }
    )


-- Representation of a node in foreign-impl layouts
newtype FNode = FNode
  { id :: String
  , deps :: Array FNode
  , time :: Number  -- meaningless except for order comparison
  , width :: Number
  , height :: Number
  }

foreign import mkNodes ::
  forall node id x.
  { getId :: node -> id
  , printId :: id -> String
  , getDepIds :: node -> Array id
  , getTime :: node -> Instant
  , getDims :: node -> { width :: Number, height :: Number }
  | x
  }
  -> Array node
  -> Object FNode

invoke
  :: forall node id. Ord node => Ord id =>
  Layout
  -> { getId :: node -> id
     , printId :: id -> String
          -- ^ must be injective
     , getDepIds :: node -> List id
     , getTime :: node -> Instant
     , getDims :: node -> { width :: Number, height :: Number }
     , nodes :: List node
     , screenDims :: { width :: Number, height :: Number }
     , focusedId :: Maybe id
     }
  -> Map id Vec2

invoke (Generic algo) info =
  algo info

invoke (Foreign algo) info =
  let

    nodes :: Object FNode
    nodes = mkNodes
              (info { getDepIds = info.getDepIds >>> Array.fromFoldable })
              (List.toUnfoldable info.nodes)

    premapping :: Object Vec2
    premapping =
      algo
        { nodes: nodes
        , screenDims: info.screenDims
        , focused: toNullable $ do
                      id <- info.printId <$> info.focusedId
                      Obj.lookup id nodes
        }
        # map toVec

    mapping :: Map id Vec2
    mapping = info.nodes
        # map (\node -> let
                          id = info.getId node
                          strId = info.printId id
                        in do pos <- Obj.lookup strId premapping
                              pure $ id /\ pos)
        # List.catMaybes
        # Map.fromFoldable

  in mapping


  where

  toVec :: { x :: Number, y :: Number } -> Vec2
  toVec = Vec2

