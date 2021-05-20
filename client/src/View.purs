module Y.Client.View (view) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold, foldl, minimumBy, length, maximum)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Data.Monoid (guard)
import Data.String.CodeUnits (length) as String
import Data.String.Common (split) as String
import Data.String.Pattern (Pattern(..)) as String
import Data.Lazy as Lazy
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)

import Html (Html)
import Html as H
import Css as S
import Attribute as A

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Convo (Message, simulate)

import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Opts (defOpts)
import Y.Client.Util.Memoize (memoizeBy)
import Y.Client.Util.Global (global)
import Y.Client.Util.OnKey (onKey, onKey'one, ShouldKeyBePressed(..), keyListenerToAttribute)
import Y.Client.Core (Model, Draft)
import Y.Client.Action (Action(..))
import Y.Client.Actions as Actions
import Y.Client.Arrange as Arrange
import Y.Client.CalcDims (calcDims)
import Y.Client.Attach ((&), (!), el)

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

  arrange = case Arrange.lookupAlgorithm model.arrangementAlgorithmKey of
        Arrange.ArrangementAlgorithm algo -> algo

  -- Extremely naughty
  -- Uses the 'viewCard' closure from the *first* render in order to calculate
  -- dimensions for that *and all subsequent* renders
  calcDims'cached :: Card -> { width :: Number, height :: Number }
  calcDims'cached = global "QdyjNN4JModpDGXRLgZn" $ Lazy.defer \_ ->
                    memoizeBy (\card -> isDraft card /\ card.id)
                              (calcDims <<< viewCard Vec2.origin)

  positions =
    arrange
      { getId: _.id }
      { getDeps: _.depIds >>> Set.toUnfoldable }
      { getTime: _.time }
      { getDims: calcDims'cached }
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
    el "div"
    & S.position "absolute"
    & S.top "0"
    & S.left "0"
    & S.width "100vw"
    & S.height "100vh"
    & S.overflow "hidden"
    & S.outline "none"  -- was getting outlined on focus
    
    & A.tabindex "0"  -- required to pick up key presses

    & (keyListenerToAttribute <<< fold)
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

              , let replies = Set.toUnfoldable (getReplies focusedCard.id) # Array.sortBy (comparing _.time)
                    mkArrowListener direction maybeCard
                      = onKey ("Arrow" <> direction) defOpts $ maybeCard # map (_.id >>> Actions.setFocused) # fromMaybe Actions.noop
                in fold
                  [ mkArrowListener "Left" $ replies Array.!! 0
                  , mkArrowListener "Down" $ replies Array.!! (length replies / 2)
                  , mkArrowListener "Right" $ replies Array.!! (length replies - 1)
                  ]
              ]
        ]

    !
    [ el "div"
      & S.position "absolute"
      & S.width "0"
      & S.height "0"
      & S.top "50vh"
      & S.left "50vw"
      & (let firstCard = cards # minimumBy (comparing _.time)
             offset = (maybeFocusedCard <|> firstCard) # map _.id >>= getPosition # fromMaybe zero # negate
         in S.transform $ "translate(" <> show (Vec2.getX offset) <> "px" <> ", " <> show (Vec2.getY offset) <> "px" <> ")")
      & S.transition "transform 0.075s ease-in-out"  -- hell yes
      !
      ( let
          arrows = cards >>= \card -> card.depIds
                                    # Set.toUnfoldable
                                    # map \dep -> { from: unsafeFromJust $ getPosition dep
                                                  , to: unsafeFromJust $ getPosition card.id }
          arrowHtmls = arrows # map viewArrow
          cardHtmls = cards # map (\card -> card # viewCard (unsafeFromJust $ getPosition card.id))

        in List.toUnfoldable (cardHtmls <> arrowHtmls) )

    , viewArrangementAlgorithmPicker model.arrangementAlgorithmKey
    ]

  viewCard :: Vec2 -> Card -> Html Action
  viewCard position card =
    el "div"
    & S.position "absolute"
    & (S.zIndex
        if isFocused card then "2"  -- above other cards
        else "1")  -- above the arrows
    & S.top (show (unwrap position).y <> "px")
    & S.left (show (unwrap position).x <> "px")
    & S.transform "translate(-50%, -50%)"  -- center the card
    & S.display "inline-block"
    & (let borderColor = if isFocused card then "red" else if isSelected card then "blue" else "lightgrey"
       in S.border $ "1px solid " <> borderColor)
    & S.padding ".8rem 1.2rem"
    & S.borderRadius ".3em"
    & S.boxShadow "0 2px 6px -4px rgb(0 0 0 / 50%)"
    & S.background "white"
    & S.fontFamily "sans-serif"
    & S.fontSize "13px"
    & S.minWidth "0ch"
    & S.maxWidth "45ch"
    & guard (not $ isDraft card) $
      S.width $ (_ <> "ch") (card.content # String.split (String.Pattern "\n") # map String.length # maximum # fromMaybe 0 # show)
    & A.onClick $ Actions.setFocused card.id
    !
    [ case card.original of
        CardOriginal_Draft _ -> mempty
        CardOriginal_Message message ->
          H.divS
          [ S.fontSize "0.75em"
          , S.fontStyle "italic"
          , S.opacity "0.5"
          , S.marginBottom "0.5em"
          ]
          [ ]
          [ H.text $ message.authorId # getAuthorName # fromMaybe "<anonymous>" ]
    , (if isDraft card then H.textareaS else H.divS)
      [ S.padding "0"
      , S.margin "0"
      , S.background "none"
      , S.resize "none"
      , S.whiteSpace "pre-line"
      , S.wordBreak "break-word"
      , S.fontFamily "inherit"
      , S.fontSize "inherit"
      , S.color "inherit"
      , S.border $ if isDraft card then "1px solid lightgrey" else "none"
      , S.outline "none !important"
      , guard (isDraft card) $ fold
        [ S.height "5em"
        , S.minWidth "300px"
        ]
      ]
      [ A.id $ "textarea-for-" <> Id.format card.id  -- hack for being able to set focus on the textareas
      , case card.original of
          CardOriginal_Draft draft -> fold
            [ onKey'one "Enter" (_ { shift = RequirePressed }) $ Actions.sendMessage draft
            , A.onInput \text -> Actions.editDraft draft.id text
            ]
          _ -> mempty
      ]
      [ H.text $ card.content ]
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

  viewArrangementAlgorithmPicker :: String -> Html Action
  viewArrangementAlgorithmPicker selection =
    H.divS
      [ S.position "absolute"
      , S.top "1rem"
      , S.right "1rem"
      , S.fontFamily "sans-serif"
      , S.fontSize "14px"
      ]
      [ ]
      [ H.text "arrangement algorithm: "
      , H.selectS
          [ S.fontFamily "inherit"
          ]
          [ A.onInput \newSelection -> Action \m -> pure $ m { arrangementAlgorithmKey = newSelection } ]
          ( Arrange.algorithms # Map.keys # Set.toUnfoldable # (["<default>"] <> _) # map \algoKey ->
                H.option
                [ if algoKey == selection then A.selected "selected" else mempty ]
                [ H.text algoKey ]
          )
      ]
