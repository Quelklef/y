module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Data.List (List)
import Data.List as List
import Data.Set (Set)
import Data.Set as Set

import Platform as Platform
import Html (Html)
import Html as H
import Attribute as A
import Css as S

type Msg = Model -> Model

type Model = Set Box

type Box =
  { id :: String
  , content :: String
  , isDraft :: Boolean
  }

type Box' =
  { box :: Box
  , content :: String
  }

mkBox' :: Box -> Box'
mkBox' box = { box, content: someRandomString }
  where someRandomString = "some random string"

initialModel :: Model
initialModel = Set.fromFoldable
  [ { id: "y-message-testing-1"
    , content: "I am message #1"
    , isDraft: false
    }
  , { id: "y-message-testing-2"
    , content: ""
    , isDraft: true
    }
  ]

main :: Effect Unit
main = do
  let app = Platform.app
        { init: \_ -> pure initialModel
        , subscriptions: \_ -> mempty
        , update: \model msg -> pure (msg model)
        , view: view
        }
  (runEffectFn1 app) unit

--

view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view boxes = { head: [], body: [bodyView] }
  where

  boxes' :: List Box'
  boxes' = Set.toUnfoldable $ Set.map mkBox' boxes

  bodyView :: Html Msg
  bodyView =
    H.div
    [ ]
    ( List.toUnfoldable $ boxes' # map viewBox' )

  viewBox' :: Box' -> Html Msg
  viewBox' box' =
    H.div
    [ ]
    [ H.p
      [ ]
      [ H.text $ if box'.box.isDraft then "DRAFT" else "MESSAGE" ]
    , H.textareaS
      [ ]
      [ A.onInput \text messages ->  messages # Set.map (\d -> if d.id == box'.box.id then d { content = text } else d) ]
      [ H.text $ box'.box.content ]
    ]


