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
import Data.Foldable (fold, foldl, minimumBy)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)

import Html (Html)
import Html as H
import Css as S
import Attribute as A

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)
import Y.Shared.Convo (Message, simulate)

import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Opts (defOpts)
import Y.Client.Util.OnKey (onKey, onKey'one, ShouldKeyBePressed(..), keyListenerToAttribute)
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

mkCard_Message :: Message -> Card
mkCard_Message m =
  { original: CardOriginal_Message m
  , id: m.id
  , depIds: m.depIds
  , content: m.content
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
  cards = ( Set.toUnfoldable $ (<>) (Set.map mkCard_Message convoState.messages) (Set.map mkCard_Draft model.drafts) )
        # List.sortBy (comparing _.id)  -- shouldn't be required; is a workaround for ursi/purescript-elmish issue #5

  cardsById :: Map (Id "Message") Card
  cardsById = cards # map (\card -> card.id /\ card) # Map.fromFoldable

  maybeFocusedCard :: Maybe Card
  maybeFocusedCard = model.focusedId >>= \id -> cardsById # Map.lookup id

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

  getReplies :: Id "Message" -> Set Card
  getReplies = \id -> Map.lookup id mapping # fromMaybe Set.empty
    where mapping = cards >>= (\card -> card.depIds # Set.toUnfoldable # map \depId -> Map.singleton depId (Set.singleton card))
                  # foldl (Map.unionWith Set.union) Map.empty

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

    , keyListenerToAttribute $ fold
        [ case maybeFocusedCard # map _.original of
            Just (CardOriginal_Draft draft) ->
               onKey "Enter" (_ { shift = RequirePressed }) $ Actions.sendMessage draft
            _ ->
               onKey "Enter" (_ { shift = RequireNotPressed }) Actions.createDraft
        , case maybeFocusedCard of
            Nothing -> mempty
            Just focusedCard -> fold
              [ onKey "ArrowUp" defOpts $ case Set.toUnfoldable focusedCard.depIds of
                  [id] -> Actions.setFocused id
                  _ -> Actions.noop
              , onKey "ArrowDown" defOpts $ case Set.toUnfoldable (getReplies focusedCard.id) of
                  [reply] -> Actions.setFocused reply.id
                  _ -> Actions.noop
              ]
        ]
    ]
    [ H.divS
      [ S.position "absolute"
      , S.width "0"
      , S.height "0"
      , S.top "50vh"
      , S.left "50vw"
      , let firstCard = cards # minimumBy (comparing _.time)
            offset = (maybeFocusedCard <|> firstCard) # map _.id >>= getPosition # fromMaybe zero # negate
        in S.transform $ "translate(" <> show (Vec2.getX offset) <> "px" <> ", " <> show (Vec2.getY offset) <> "px" <> ")"
      , S.transition "transform 0.075s ease-in-out"  -- hell yes
      ]
      [ ]
      $ let
          arrows = cards >>= \card -> card.depIds
                                    # Set.toUnfoldable
                                    # map \dep -> { from: unsafeFromJust $ getPosition dep
                                                  , to: unsafeFromJust $ getPosition card.id }
          arrowHtmls = arrows # map viewArrow
          cardHtmls = cards # map (\card -> card # viewCard (unsafeFromJust $ getPosition card.id))

        in List.toUnfoldable (cardHtmls <> arrowHtmls)
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
    [ A.onClick $ Actions.setFocused card.id ]
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
              [ onKey'one "Enter" (_ { shift = RequirePressed }) $ Actions.sendMessage draft
              , A.onInput \text -> Actions.editDraft draft.id text
              ]
        ]
        [ H.text $ card.content
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

  viewArrow :: forall msg. { from :: Vec2, to :: Vec2 } -> Html msg
  viewArrow { from, to } =
    H.spanS
      [ S.display "inline-block"
      , S.position "absolute"
      , S.background "url(data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAoAAAAKCAYAAACNMs+9AAAAOUlEQVQoU2N89uzZfwY0ICUlxYguxohNIUgRumKwTmIUw60gpBjFLfgUk66QKKsJKQJ5mPjgITbAAdzAKAuIG+NRAAAAAElFTkSuQmCC) repeat"
      , S.height "10px"
      , S.width $ show (Vec2.mag $ to - from) <> "px"
      , S.top $ show (Vec2.getY from) <> "px"
      , S.left $ show (Vec2.getX from) <> "px"
      , S.transform $ "rotate(" <> show (Vec2.angle $ to - from) <> "rad)"
      , S.transformOrigin "center left"
      ]
      [ ]
      [ ]
