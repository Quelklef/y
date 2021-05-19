module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple.Nested ((/\))
import Data.Set (Set)
import Data.Set as Set
import Data.Newtype (wrap, unwrap)

import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

import Sub (Sub)
import Html (Html)
import Html as H
import Attribute as A
import Css as S

import Y.Shared.Id (Id, newId)
import Y.Shared.Convo (Event)
import Y.Shared.Transmission (Transmission(..))
import Y.Shared.Config as Config

import Y.Client.App (runApp)
import Y.Client.WebSocket as Ws
import Y.Client.WebSocketClientToElmishSubscription (websocketClientToElmishSubscription)

type Action = Model -> Effect Model

type Message =
  { id :: Id "Message"
  , content :: String
  , isDraft :: Boolean
  }

type Model =
  { messages :: Set Message
  }

mkInitialModel :: Model
mkInitialModel =
  { messages: Set.fromFoldable
    [ { id: wrap "y-message-testing-1"
      , content: "I am message #1"
      , isDraft: false
      }
    , { id: wrap "y-message-testing-2"
      , content: ""
      , isDraft: true
      }
    ]
  }

main :: Effect Unit
main = do

  -- Initialize state
  let initialModel = mkInitialModel

  -- Start Elmish
  runApp
    { initialModel: initialModel
    , subscriptions: const mempty
    , view: view
    , interpret: identity
    }

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

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = unsafePartial $ fromJust m

view :: Model -> { head :: Array (Html Action), body :: Array (Html Action) }
view model = { head: [], body: [bodyView] }
  where

  cards :: List Card
  cards = Set.toUnfoldable $ Set.map mkCard_Message model.messages

  bodyView :: Html Action
  bodyView =
    H.div
    [ ]
    ( List.toUnfoldable $ cards # map viewCard )

  viewCard :: Card -> Html Action
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
      [ case card.original of
          draft ->
            A.onInput \text m -> pure $ m { messages = m.messages # Set.map (\d -> if d.id == draft.id then d { content = text } else d) }
      ]
      [ H.text $ getContent card
      ]
    ]


