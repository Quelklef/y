module Y.Client.CalcDims where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (runEffectFn1)

import Html (Html)
import Html as H
import Attribute as A
import Platform as Platform

import Y.Shared.Id (Id)
import Y.Shared.Id as Id

-- | An extremely naughty function which magically computes a node's rendered dimensions size
calcDims :: forall msg. Html msg -> { width :: Number, height :: Number }
calcDims = unsafePerformEffect do
  pure $ \html -> unsafePerformEffect do
    (divId :: Id "naughtydivtags") <- Id.new
    let wrap = H.div [ A.id $ Id.format divId ] [ html ]
    let app = Platform.app
                { init: \_ -> pure unit
                , update: \model _ -> pure model  -- ignore all messages!
                , subscriptions: \_ -> mempty
                , view: pure { head: [], body: [wrap] }
                }
    runEffectFn1 app unit  -- render the node to the body element
    dims <- getTheDimsAndCleanUp $ Id.format divId
    pure dims

foreign import getTheDimsAndCleanUp :: String -> Effect { width :: Number, height :: Number }
