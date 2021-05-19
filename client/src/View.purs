module Y.Client.View (view) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold, minimumBy)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)

import Html (Html)
import Html as H
import Css as S
import Attribute as A
import WHATWG.DOM.Event (target, currentTarget) as Wwg
import WHATWG.HTML.KeyboardEvent (toMaybeKeyboardEvent, shiftKey, key) as Wwg

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)

import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Is ((===))
import Y.Client.Util.Opts (Opts)
import Y.Client.Core (Model, Message)
import Y.Client.Action (Action)
import Y.Client.Actions as Actions
import Y.Client.Arrange (arrange)
import Y.Client.CalcDims (calcDims)

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


