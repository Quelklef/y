module Y.Client.Layouts.Dagre where

import Foreign.Object (Object)
import Data.Nullable (Nullable)

import Y.Client.Layouts.Core (Layout (Foreign), FNode)

layout :: Layout
layout = Foreign arrange_f

foreign import arrange_f ::
  forall x.
  { nodes :: Object FNode  -- maps id to node
  , screenDims :: { width :: Number, height :: Number }
  , focused :: Nullable FNode
  | x
  }
  -> Object { x :: Number, y :: Number }

