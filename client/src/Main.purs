module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Data.Maybe (Maybe(..))
import Data.Foldable (fold, length, intercalate)
import Data.Array (slice)

import Platform as Platform
import Html (Html)
import Html as H
import Css as S
import Attribute as A
import Attribute (Attribute)
import WHATWG.HTML.KeyboardEvent (toMaybeKeyboardEvent, key) as Wwg

type Model =
  { directions :: Array String
  , lastPressed :: String
  }

data Msg = Msg_Noop | Msg_RotateDirections | Msg_Set String

initialModel :: Model
initialModel =
  { directions: ["Up", "Down", "Left", "Right"]
  , lastPressed: "<none>"
  }

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
update model Msg_Noop = model
update model Msg_RotateDirections =
  model { directions =  slice 1 (length model.directions) model.directions <> slice 0 1 model.directions }
update model (Msg_Set lastPressed) = model { lastPressed = lastPressed }

view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view model =
  { head: []
  , body:
    [ H.p [ ]
      [ H.text $ "Order of event listeners is: " <> (model.directions # intercalate ", ")
      , H.text " "
      , H.button [ A.onClick Msg_RotateDirections ] [ H.text "rotate" ]
      ]
    , H.divS
      [ S.backgroundColor "rgba(0 200 0 / 20%)" ]
      [ A.tabindex "0" {- required to pick up keypresses -}
      , fold $ model.directions # map \dir -> onKey ("Arrow" <> dir) (Msg_Set dir)
      ]
      [ H.text "Click here then press an arrow key" ]
    , H.p [ ] [ H.text $ "The last arrow key pressed was: " <> model.lastPressed ]
    ]
  }

onKey :: String -> Msg -> Attribute Msg
onKey key msg =
  A.on "keydown" \event -> pure $
    case Wwg.toMaybeKeyboardEvent event of
      Nothing -> Msg_Noop
      Just keyEvent -> do
        let keyOk = Wwg.key keyEvent == key
        if keyOk then msg else Msg_Noop
