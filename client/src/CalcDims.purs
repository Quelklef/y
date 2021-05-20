module Y.Client.CalcDims where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)

import Html (Html)
import Platform as Platform
import WHATWG.HTML.All (Element)

-- | An extremely naughty function which magically computes a node's rendered dimensions size
calcDims :: forall msg. Html msg -> { width :: Number, height :: Number }
calcDims = unsafePerformEffect do
  head <- createOffPageElement
  body <- createOffPageElement
  pure $ \html -> unsafePerformEffect do
    let app = Platform.headBodyApp
                { init: \_ -> pure unit
                , update: \model _ -> pure model  -- ignore all messages!
                , subscriptions: \_ -> mempty
                , view: pure { head: [], body: [html] }
                , head: head
                , body: body
                }
    runEffectFn1 app unit  -- render the node to the body element
    rendered <- getOnlyChildOfElement body
    dims <- getElementDims rendered
    deleteElement rendered
    pure dims

foreign import createOffPageElement :: Effect Element
foreign import getOnlyChildOfElement :: Element -> Effect Element
foreign import getElementDims :: Element -> Effect { width :: Number, height :: Number }
foreign import deleteElement :: Element -> Effect Unit
