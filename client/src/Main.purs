module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Partial.Unsafe (unsafePartial)

import Platform as Platform
import Html (Html)
import Html as H
import Attribute as A

type Id = Int

type Model = Array Box
data Msg = Msg_SwapBoxesAndSetText Id String

type Box =
  { id :: Id
  , text :: String
  , label :: String
  }

type Box' =
  { box :: Box
  , text :: String
  }

mkBox' :: Box -> Box'
mkBox' box = { box, text: someRandomString }
  where someRandomString = "some random string"

initialModel :: Model
initialModel =
  [ { id: 1
    , label: "Box #1"
    , text: "text A"
    }
  , { id: 2
    , label: "Box #2"
    , text: "text B"
    }
  ]

main :: Effect Unit
main = do
  let app = Platform.app
        { init: \_ -> pure initialModel
        , subscriptions: \_ -> mempty
        , update: \model msg -> pure (update model msg)
        , view: view
        }
  (runEffectFn1 app) unit

update :: Model -> Msg -> Model
update boxes (Msg_SwapBoxesAndSetText targetBoxId newText) =
  boxes # map updateBox # swapOrder
  where
    updateBox box = if box.id == targetBoxId then box { text = newText } else box
    swapOrder = unsafePartial $ \[a, b] -> [b, a]


view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view boxes =
  { head: []
  , body: boxes # map mkBox' # map viewBox'
  }

viewBox' :: Box' -> Html Msg
viewBox' box' =
  H.div
  [ ]
  [ H.p
    [ ]
    [ H.text $ box'.box.label ]
  , H.textarea
    [ A.onInput \text -> Msg_SwapBoxesAndSetText box'.box.id text ]
    [ H.text $ box'.box.text ]
  ]


