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
import Y.Shared.Convo (Message, simulate)

import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Is ((===))
import Y.Client.Util.Opts (Opts)
import Y.Client.Core (Model, Draft)
import Y.Client.Action (Action)
import Y.Client.Actions as Actions
import Y.Client.Arrange (arrange)
import Y.Client.CalcDims (calcDims)

-- A card is a message or a draft plus computed info such as the shared fields
-- The real solution here would be to use lenses
data CardOriginal = CardOriginal_Message Message | CardOriginal_Draft Draft
type Card =
  { original :: CardOriginal
  , id :: Id "Message"
  , depIds :: Set (Id "Message")
  , content :: String
  , time :: Instant  -- time created (draft) or sent (message)
  }

-- Temporary replacement for _.content
getContent :: Card -> String
getContent card = case card.original of
  CardOriginal_Draft d -> d.content
  CardOriginal_Message m -> m.content

mkCard_Message :: Message -> Card
mkCard_Message m =
  { original: CardOriginal_Message m
  , id: m.id
  , depIds: m.depIds
  , content: "<any string>"
  , time: m.timeSent
  }

mkCard_Draft :: Draft -> Card
mkCard_Draft d =
  { original: CardOriginal_Draft d
  , id: d.id
  , depIds: d.depIds
  , content: d.content
  , time: d.timeCreated
  }

derive instance genericCardOriginal :: Generic CardOriginal _
derive instance eqCardOriginal :: Eq CardOriginal
derive instance ordCardOriginal :: Ord CardOriginal

--

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = unsafePartial $ fromJust m

view :: Model -> { head :: Array (Html Action), body :: Array (Html Action) }
view model = { head: [], body: [bodyView] }
  where

  convoState = simulate model.convo.events

  cards :: List Card
  cards = Set.toUnfoldable $ (<>) (Set.map mkCard_Message convoState.messages) (Set.map mkCard_Draft model.drafts)

  cardsById :: Map (Id "Message") Card
  cardsById = cards # map (\card -> card.id /\ card) # Map.fromFoldable

  focusedCard :: Maybe Card
  focusedCard = model.focusedId >>= \id -> cardsById # Map.lookup id

  isFocused :: forall r. { id :: Id "Message" | r } -> Boolean
  isFocused { id } = model.focusedId == Just id

  isSelected :: forall r. { id :: Id "Message" | r } -> Boolean
  isSelected { id } = model.selectedIds # Set.member id

  isDraft :: Card -> Boolean
  isDraft card = case card.original of
    CardOriginal_Draft _ -> true
    CardOriginal_Message _ -> false

  positions =
    arrange
      { getId: _.id }
      { getDeps: _.depIds >>> Set.toUnfoldable }
      { getTime: _.time }
      { getDims: calcDims <<< viewCard Vec2.origin }
      cards

  getPosition :: Id "Message" -> Maybe Vec2
  getPosition id = positions # Map.lookup id

  getAuthorName :: Id "User" -> Maybe String
  getAuthorName id = convoState.userNames # Map.lookup id

  bodyView :: Html Action
  bodyView =
    H.divS
    [ S.position "absolute"
    , S.top "0"
    , S.left "0"
    , S.width "100vw"
    , S.height "100vh"
    , S.overflow "hidden"
    , S.outline "none"  -- was getting outlined on focus
    ]
    [ A.tabindex "0"  -- required to pick up key presses
    ]
    [ H.divS
      [ S.position "absolute"
      , S.width "0"
      , S.height "0"
      , S.top "50vh"
      , S.left "50vw"
      , let firstCard = cards # minimumBy (comparing _.time)
            offset = (focusedCard <|> firstCard) # map _.id >>= getPosition # fromMaybe zero # negate
        in S.transform $ "translate(" <> show (Vec2.getX offset) <> "px" <> ", " <> show (Vec2.getY offset) <> "px" <> ")"
      , S.transition "transform 0.075s ease-in-out"  -- hell yes
      ]
      [ ]
      $ let
          cardHtmls = cards # map (\card -> card # viewCard (unsafeFromJust $ getPosition card.id))

        in List.toUnfoldable cardHtmls
    ]

  viewCard :: Vec2 -> Card -> Html Action
  viewCard position card =
    H.divS
    [ S.position "absolute"
    , S.zIndex $
        if isFocused card then "2"  -- above other cards
        else "1"  -- above the arrows
    , S.top $ (show (unwrap position).y) <> "px"
    , S.left $ (show (unwrap position).x) <> "px"
    , S.transform "translate(-50%, -50%)"  -- center the card
    , S.width "300px"
    , S.height "auto"
    , S.display "inline-block"
    , let borderColor = if isFocused card then "red" else if isSelected card then "blue" else "lightgrey"
      in S.border $ "1px solid " <> borderColor
    , S.padding ".8rem 1.2rem"
    , S.borderRadius ".3em"
    , S.boxShadow "0 2px 6px -4px rgb(0 0 0 / 50%)"
    , S.background "white"
    , S.fontFamily "sans-serif"
    , S.fontSize "13px"
    ]
    [ A.onClick $ pure <<< (_ { focusedId = Just card.id }) ]
    [ H.divS
      [ ]
      [ ]
      [ H.textareaS
        [ S.padding "0"
        , S.background "none"
        , S.resize "none"
        , S.fontFamily "inherit"
        , S.fontSize "inherit"
        , S.color "inherit"
        , S.border $ if isDraft card then "1px solid lightgrey" else "none"
        , S.outline "none !important"
        , S.width "100%"
        ]
        [ A.id $ "textarea-for-" <> unwrap card.id  -- hack for being able to set focus on the textareas
        , case card.original of
            CardOriginal_Message _ ->
              A.disabled "disabled"

            CardOriginal_Draft draft -> fold
              [ A.onInput \text -> Actions.editDraft draft.id text
              ]
        ]
        [ H.text $ getContent card
        ]
      ]
    , H.divS
      [ S.fontSize "0.75em"
      , S.textAlign "right"
      , S.fontStyle "italic"
      , S.opacity "0.5"
      ]
      [ ]
      [ case card.original of
          CardOriginal_Draft d ->
            mempty
          CardOriginal_Message m ->
            H.text $ (unwrap card.id) <> " from " <> (m.authorId # getAuthorName # fromMaybe "<anonymous>")
      ]
    ]


