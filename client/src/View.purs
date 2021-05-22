module Y.Client.View (view) where

import Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Int as Int
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold, foldl, minimumBy, length, maximum, foldMap)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Data.Monoid (guard)
import Data.String.CodeUnits (length) as String
import Data.String.Common (trim, split) as String
import Data.String.Pattern (Pattern(..)) as String
import Data.Lazy as Lazy
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)

import Html (Html)
import Html as H
import Css as S
import Attribute as A
import WHATWG.HTML.KeyboardEvent (toMaybeKeyboardEvent, shiftKey, key) as Wwg
import WHATWG.DOM.Event (stopPropagation) as Wwg

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Convo (EventPayload(..), Message, simulate)

import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Memoize (memoizeBy)
import Y.Client.Util.Global (global)
import Y.Client.Core (Model, Draft)
import Y.Client.Action (Action(..))
import Y.Client.Actions as Actions
import Y.Client.Arrange (algorithms) as Arrange
import Y.Client.ArrangementAlgorithms.Types (ArrangementAlgorithm(..)) as Arrange
import Y.Client.CalcDims (calcDims)
import Y.Client.Colors as Colors

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
view model = { head: headView, body: [bodyView] }
  where

  convoState = simulate model.convo.events

  cards :: List Card
  cards = ( Set.toUnfoldable $ (<>) (Set.map mkCard_Message convoState.messages) (Set.map mkCard_Draft model.drafts) )
        # List.sortBy (comparing _.id)  -- shouldn't be required; is a workaround for ursi/purescript-elmish issue #5

  cardsById :: Map (Id "Message") Card
  cardsById = cards # map (\card -> card.id /\ card) # Map.fromFoldable

  getAuthorId :: Card -> Id "User"
  getAuthorId card = case card.original of
    CardOriginal_Message m -> m.authorId
    CardOriginal_Draft _ -> model.userId

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

  arrange = case unsafeFromJust $ Map.lookup model.arrangementAlgorithmKey Arrange.algorithms of
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
      { getId: _.id
      , getDepIds: _.depIds >>> Set.toUnfoldable
      , getTime: _.time
      , getDims: calcDims'cached
      , nodes: cards
      }

  getPosition :: Id "Message" -> Maybe Vec2
  getPosition id = positions # Map.lookup id

  getAuthorName :: Id "User" -> String
  getAuthorName id = convoState.userNames # Map.lookup id # fromMaybe "<anonymous>"

  getReplies :: Id "Message" -> Set Card
  getReplies = \id -> Map.lookup id mapping # fromMaybe Set.empty
    where mapping = cards >>= (\card -> card.depIds # Set.toUnfoldable # map \depId -> Map.singleton depId (Set.singleton card))
                  # foldl (Map.unionWith Set.union) Map.empty

  getUserColor :: Id "User" -> String
  getUserColor id = List.findIndex (_ == id) userIdsChronologically # map (Colors.make seed) # fromMaybe "black"
    where
      seed = Id.format model.convo.id
      userIdsChronologically = Map.keys userIdToFirstMessageTime
                             # Set.toUnfoldable
                             # List.sortBy (comparing $ flip Map.lookup userIdToFirstMessageTime)
      userIdToFirstMessageTime = model.convo.events
                               # map (\event -> case event.payload of
                                   EventPayload_MessageSend pl -> Map.singleton pl.message.authorId event.time
                                   EventPayload_SetName _ -> Map.empty)
                               # foldl Map.union Map.empty  -- left-biased

  headView :: Array (Html Action)
  headView =
    [ H.element "style" [] [ H.text $ fold
        -- Instead of using inline CSS, Elmish assigns classes to nodes and then doles
        -- out styles via those classes. During a re-render, the node classes might
        -- change. This is generally not an issue, but can break CSS transitions.
        -- We get around this limitation by defining our CSS transitions in CSS classes,
        -- rather that inline on the nodes, and then attach the nodes to the classes.
        -- TODO: re-inline this style once Elmish is patched.
        [ ".pan-animation-workaround { transition: transform 0.075s ease-in-out; }"

        -- border box best sizing
        , "* { box-sizing: border-box }"

        -- colorshift animation
        , let granularity = 10
              keyframes =
                Array.range 0 granularity
                # map (\n ->
                  let t = Int.toNumber n / Int.toNumber granularity
                  in show (t * 100.0) <> "% { background: hsl(" <> show (t * 360.0) <> " 80% 90%); }")
                # fold
          in "@keyframes colorshift-anim {" <> keyframes <> "}"
        , ".colorshift { animation: colorshift-anim 15s; animation-iteration-count: infinite; }"
        ]
      ]
    ]

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

    , A.on "keydown" \event -> pure $
        Wwg.toMaybeKeyboardEvent event # foldMap \keyEvent ->

          case maybeFocusedCard of
            Nothing ->
              guard (Wwg.key keyEvent == "Enter") Actions.createDraft

            Just focusedCard ->
              -- send/create draft on (shift-)enter
              if Wwg.key keyEvent == "Enter" then
                case focusedCard.original of
                  CardOriginal_Message _ -> Actions.createDraft
                  _ -> Actions.noop

              -- Select card on E
              else if Wwg.key keyEvent == "e" then
                Actions.setSelected focusedCard.id (not $ isSelected focusedCard)

              -- arrow key controls
              else if Wwg.key keyEvent == "ArrowUp" then
                case Set.toUnfoldable focusedCard.depIds of
                  [id] -> Actions.setFocused id
                  _ -> Actions.noop

              else if ["ArrowLeft", "ArrowRight", "ArrowDown"] # Array.elem (Wwg.key keyEvent) then
                let replies = Set.toUnfoldable (getReplies focusedCard.id) # Array.sortBy (comparing _.time)
                in [ "ArrowLeft" /\ { to: 0 }
                   , "ArrowDown" /\ { to: length replies / 2 }
                   , "ArrowRight" /\ { to: length replies - 1 }
                   ]
                   # foldMap \(key /\ { to }) ->
                      guard (Wwg.key keyEvent == key) $
                      replies Array.!! to # foldMap (_.id >>> Actions.setFocused)

              else Actions.noop
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
      ]
      [ A.addClass "pan-animation-workaround"
      ]
      $ let
          arrows = cards >>= \card -> card.depIds
                                    # Set.toUnfoldable
                                    # map \dep -> { from: unsafeFromJust $ getPosition dep
                                                  , to: unsafeFromJust $ getPosition card.id }
          arrowHtmls = arrows # map viewArrow
          cardHtmls = cards # map (\card -> card # viewCard (unsafeFromJust $ getPosition card.id))

        in List.toUnfoldable (cardHtmls <> arrowHtmls)

    , H.divS
      [ S.position "absolute"
      , S.top ".5rem"
      , S.right "1rem"
      , S.fontFamily "sans-serif"
      , S.fontSize "14px"
      , S.textAlign "right"
      , S.lineHeight "2rem"
      ]
      [ ]
      [ viewArrangementAlgorithmPicker model.arrangementAlgorithmKey
      , viewNameChanger
      ]
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
    , S.display "inline-block"
    , S.border $ "1px solid " <> (if isFocused card then "red" else "lightgrey")
    , S.background "white"
    , S.padding ".8rem 1.2rem"
    , S.borderRadius ".3em"
    , S.boxShadow "0 2px 6px -4px rgb(0 0 0 / 50%)"
    , S.fontFamily "sans-serif"
    , S.fontSize "13px"
    , S.minWidth "18ch"
    , S.maxWidth "35ch"
    , S.width $ if isDraft card
      then "100vw"  -- i.e., max-width
      else (_ <> "ch") (card.content # String.split (String.Pattern "\n") # map String.length # maximum # fromMaybe 0 # show)
    , S.outline "none"
    ]
    [ guard (isSelected card) $ A.addClass "colorshift"
    , A.onClick $ Actions.setFocused card.id
    ]
    [ case card.original of
        CardOriginal_Draft _ -> mempty
        CardOriginal_Message message ->
          H.divS
          [ S.fontSize "0.75em"
          , S.fontStyle "italic"
          , S.opacity "0.5"
          , S.marginBottom "0.5em"
          , S.paddingTop "1px"
          ]
          [ ]
          [ H.spanS
            [ S.height "0.5em"
            , S.width "0.5em"
            , S.backgroundColor $ getUserColor (getAuthorId card)
            , S.border "1px solid black"
            , S.marginRight "0.5em"
            , S.display "inline-block"
            ]
            [ ]
            [ ]
          , H.text $ message.authorId # getAuthorName
          ]
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
        , S.width "100%"
        ]
      ]
      [ A.id $ "textarea-for-" <> Id.format card.id  -- hack for being able to set focus on the textareas
      , A.value $ card.content
      , case card.original of
          CardOriginal_Draft draft -> fold
            [ A.onInput \text -> Actions.editDraft draft.id text
            , A.on "keydown" \event ->
                Wwg.toMaybeKeyboardEvent event # foldMap \keyEvent -> do
                  Wwg.stopPropagation keyEvent
                  let trimmed = draft { content = String.trim draft.content }
                      ok = Wwg.key keyEvent == "Enter"
                           && Wwg.shiftKey keyEvent
                           && trimmed.content /= ""
                      action = guard ok $ Actions.sendMessage trimmed
                  pure action
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
    H.div
      [ ]
      [ H.text "arrangement algorithm: "
      , H.selectS
          [ S.fontFamily "inherit"
          ]
          [ A.onInput \newSelection -> Action \m -> pure $ m { arrangementAlgorithmKey = newSelection } ]
          ( Arrange.algorithms # Map.keys # Set.toUnfoldable # map \algoKey ->
                H.option
                [ if algoKey == selection then A.selected "selected" else mempty ]
                [ H.text algoKey ]
          )
      ]

  viewNameChanger ::  Html Action
  viewNameChanger =
    H.div
      [ ]
      [ H.text "nickname: "
      , H.inputS
        [ S.width "15ch" ]
        [ A.value $ model.nicknameInputValue # fromMaybe (getAuthorName model.userId)
        , A.onInput \text -> Action \m -> pure $ m { nicknameInputValue = Just text }
        ]
      , H.text " "
      -- v TODO: The existence of these buttons is unfortunate.
      --         Ideally, the nickname <input> would react to changes by debouncing and then
      --         invoking Actions.setName, i.e., invoking a debounced Actions.setName.
      --         Unfortunately, an Action is required to be synchronous, meaning that it
      --         cannot be debounced. ActionMonad will need to be upgraded before actions
      --         can be debounced.
      --         This is likely best resolved by wrapping it in some kind of ContT.
      --         I have not done this yet because it is going to be difficult and because
      --         this codebase is behind an Elmish version or two anyway.
      , H.button
        [ guard (isNothing model.nicknameInputValue) $ A.disabled "disabled"
        , A.onClick $ foldMap Actions.setName model.nicknameInputValue ]
        [ H.text "update" ]
      , H.text " "
      , H.button
        [ guard (isNothing model.nicknameInputValue) $ A.disabled "disabled"
        , A.onClick $ Action \m -> pure $ m { nicknameInputValue = Nothing }
        ]
        [ H.text "cancel" ]
      ]
