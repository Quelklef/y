module Client.App (runApp) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Data.Tuple.Nested (type (/\), (/\))
import Control.Monad.Trans.Class (lift)

import Platform as Platform
import Sub (Sub)
import Html (Html)

-- Ostensibly, this codebase uses Elmish.
-- This is true. However, we don't use a typical Elmlike architecture.
-- This module encapsulates our custom architecture

type Action model effect = model -> effect model

runApp :: forall model effect.
          { initialModel :: model
          , subscriptions :: model -> Sub (Action model effect)
          , view :: model ->
                    { head :: Array (Html (Action model effect))
                    , body :: Array (Html (Action model effect))
                    }
          , interpret :: effect ~> Effect
          }
          -> Effect Unit

runApp args = do
  let elmish = Platform.app
        { init: \_ -> pure args.initialModel
        , update: \model action -> lift (args.interpret (action model))
        , subscriptions: \model -> args.subscriptions model
        , view: \model -> args.view model
        } 
  (runEffectFn1 elmish) unit
