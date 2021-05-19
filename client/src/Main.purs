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

type Model = Set Message

type Message =
  { id :: String
  , content :: String
  , isDraft :: Boolean
  }

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

-- A card is a message or a draft plus computed info such as the shared fields
-- The real solution here would be to use lenses
type Card =
  { original :: Message
  , content :: String
  }

-- Temporary replacement for _.content
getContent :: Card -> String
getContent card = card.original.content

mkCard_Message :: Message -> Card
mkCard_Message m =
  { original: m
  , content: "<any string>"
  }

--

view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view model = { head: [], body: [bodyView] }
  where

  cards :: List Card
  cards = Set.toUnfoldable $ Set.map mkCard_Message model

  bodyView :: Html Msg
  bodyView =
    H.div
    [ ]
    ( List.toUnfoldable $ cards # map viewCard )

  viewCard :: Card -> Html Msg
  viewCard card =
    H.divS
    [ S.width "300px"
    , S.height "auto"
    , S.border $ "1px solid black"
    , S.padding ".8rem 1.2rem"
    , S.margin "40px 0"
    ]
    [ ]
    [ H.p
      [ ]
      [ H.text $ if card.original.isDraft then "DRAFT" else "MESSAGE" ]
    , H.textareaS
      [ S.padding "0"
      , S.background "none"
      , S.resize "none"
      , S.fontFamily "inherit"
      , S.fontSize "inherit"
      , S.color "inherit"
      , S.outline "none !important"
      , S.width "100%"
      ]
      [ A.onInput \text messages ->  messages # Set.map (\d -> if d.id == card.original.id then d { content = text } else d)
      ]
      [ H.text $ getContent card
      ]
    ]


