module Main where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1)
import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set

import Platform as Platform
import Html (Html)
import Html as H
import Attribute as A

type Id = String

type Model = Set Box
data Msg = Msg_SetContent Id String

type Box =
  { id :: Id
  , content :: String
  , label :: String
  , order :: Int
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
    , label: "Box #1"
    , content: "I am message #1"
    , order: 2
    }
  , { id: "y-message-testing-2"
    , label: "Box #2"
    , content: ""
    , order: 1
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
update boxes (Msg_SetContent boxId newContent) = boxes # Set.map updateBox
  where
    updateBox = setOrder >>> setContent
    setOrder box = let newOrder = if box.order == 1 then 2 else 1 in box { order = newOrder }
    setContent box = if box.id == boxId then box { content = newContent } else box


view :: Model -> { head :: Array (Html Msg), body :: Array (Html Msg) }
view boxes =
  { head: []
  , body: boxes # Set.toUnfoldable # Array.sortBy (comparing _.order) # map mkBox' # map viewBox'
  }

viewBox' :: Box' -> Html Msg
viewBox' box' =
  H.div
  [ ]
  [ H.p
    [ ]
    [ H.text $ box'.box.label ]
  , H.textarea
    [ A.onInput \text -> Msg_SetContent box'.box.id text ]
    [ H.text $ box'.box.content ]
  ]


