module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Partial.Unsafe (unsafePartial)

import Platform as Platform
import Html (Html)
import Html as H
import Css as S
import Attribute as A

type Id = Int

type Model = Array Box
data Msg = Msg_SwapBoxesAndSetText Id String

type Box = { id :: Id , text :: String }

initialModel :: Model
initialModel =
  [ { id: 1, text: "text A" }
  , { id: 2, text: "text B" }
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
  , body:
      [ H.button [ A.onClick $ Msg_SwapBoxesAndSetText 1 "new text" ] [ H.text "swap boxes and set box #1 text to 'new text'" ] ]
      <>
      ( boxes # map \box ->
          H.divS
          [ S.backgroundColor $ if box.id == 1 then "rgba(200 0 0 / 20%)" else "rgba(0 200 0 / 20%)" ]
          [ ]
          [ H.p
            [ ]
            [ H.text $ "Box #" <> show box.id ]
          , H.textarea
            [ A.onInput \text -> Msg_SwapBoxesAndSetText box.id text ]
            [ H.text $ box.text ]
          ]
      )
  }
