module Client.View (view) where

import Prelude

import Debug as Debug

import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Foldable (fold)
import Data.Monoid (guard)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Partial.Unsafe (unsafePartial)

import Html (Html)
import Html as H
import Css as S
import Attribute as A
import WHATWG.DOM.Event (target, currentTarget) as Wwg
import WHATWG.HTML.KeyboardEvent (toMaybeKeyboardEvent, shiftKey, key) as Wwg

import Shared.Id (Id)
import Shared.Convo (Message, simulate)

import Client.Util.Vec2 (Vec2)
import Client.Util.Vec2 as Vec2
import Client.Util.Is ((===))
import Client.Util.Opts (Opts, defOpts)
import Client.Core (Model, Draft)
import Client.Action (Action)
import Client.Actions as Actions
import Client.Arrange (arrange)
import Client.CalcDims (calcDims)

data Card = Card_Message Message | Card_Draft Draft

-- Mason's solution to the card problem
--uncard :: ({ id ::, deps ::, content :: } -> r) -> Card -> r
--uncard f (Card_Message m) = f m
--uncard f (Card_Draft f) = f d

-- Another solution
--class Card c where
--  id ::
--  deps ::
--  content ::

derive instance genericCard :: Generic Card _
derive instance eqCard :: Eq Card
derive instance ordCard :: Ord Card

-- Hmmm, maybe there's a nicer way to this card* stuff.
-- ... typeclasses?

cardId :: Card -> Id "Message"
cardId (Card_Message m) = m.id
cardId (Card_Draft d) = d.id

cardDeps :: Card -> Set (Id "Message")
cardDeps (Card_Message m) = m.deps
cardDeps (Card_Draft d) = d.deps

cardContent :: Card -> String
cardContent (Card_Message m) = m.content
cardContent (Card_Draft d) = d.content

cardAuthorId :: Card -> Maybe (Id "User")
cardAuthorId (Card_Message m) = Just m.authorId
cardAuthorId (Card_Draft _) = Nothing

isDraft :: Card -> Boolean
isDraft (Card_Message _) = false
isDraft (Card_Draft _) = true

view :: Model -> { head :: Array (Html Action), body :: Array (Html Action) }
view model = { head: [], body: [viewBody model] }

viewBody :: Model -> Html Action
viewBody model =
  let
    convoState = simulate model.convo.events
    (cards :: List Card) = Set.toUnfoldable $ (<>) (Set.map Card_Message convoState.messages) (Set.map Card_Draft model.drafts)

    viewCard'needsPosition position card =
      viewCard
        convoState.userNames
        (model.focused == Just (cardId card))
        (model.selected # Set.member (cardId card))
        position
        card

    positions =
      arrange
        cardId
        (cardDeps >>> Set.toUnfoldable)
        (case _ of
            Card_Message m -> m.time
            Card_Draft d -> d.timeCreated)
        (\card -> calcDims $ viewCard'needsPosition Vec2.origin card)
        cards

    cardHtmls = cards # map \card -> let position = unsafePartial $ fromJust (Map.lookup (cardId card) positions)
                                     in viewCard'needsPosition position card

    arrows = cards >>= \card -> cardDeps card # Set.toUnfoldable # map \dep ->
      let from = unsafePartial $ fromJust $ positions # Map.lookup dep
          to   = unsafePartial $ fromJust $ positions # Map.lookup (cardId card)
      in { from, to }

    arrowHtmls = arrows # map viewArrow

  in
    H.divS
    [ S.position "absolute"
    , S.top "0"
    , S.left "0"
    , S.width "100vw"
    , S.height "100vh"
    ]
    [ guard (isNothing model.focused) $ onKey "Enter" defOpts pure Actions.createDraft
    , A.tabindex "0"  -- required to pick up key presses
    ]
    [ H.divS
      [ S.position "absolute"
      , S.top "50vh"
      , S.left "50vw"
      , S.width "0"
      , S.height "0"
      ]
      [ ]
      (List.toUnfoldable $ cardHtmls <> arrowHtmls)
    ]

viewCard :: Map (Id "User") String -> Boolean -> Boolean -> Vec2 -> Card -> Html Action
viewCard userNames isFocused isSelected position card =
  H.divS
  [ S.position "absolute"
  , S.zIndex "1"  -- above the arrows
  , S.top $ (show (unwrap position).y) <> "px"
  , S.left $ (show (unwrap position).x) <> "px"
  , S.transform "translate(-50%, -50%)"  -- center the card
  , S.width "300px"
  , S.height "auto"
  , S.display "inline-block"

  , let borderColor = if isFocused then "red" else if isSelected then "blue" else "lightgrey"
    in S.border $ "1px solid " <> borderColor
  , S.padding ".8rem 1.2rem"
  , S.borderRadius ".3em"
  , S.boxShadow "0 2px 6px -4px rgb(0 0 0 / 50%)"
  , S.background "white"
  , S.fontFamily "sans-serif"
  , S.fontSize "13px"
  ]
  [ A.onClick \model -> pure $ model { focused = Just (cardId card) }
  , guard (not $ isDraft card) $ onKey "Enter" defOpts pure Actions.createDraft
  ]
  [ H.divS
    [ ]
    [ A.tabindex "0"  -- required to pick up key presses
    ]
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
      [ A.id $ "textarea-for-" <> unwrap (cardId card)  -- hack for being able to set focus on the textareas
      , case card of
          Card_Message _ ->
            A.disabled "disabled"

          Card_Draft draft -> fold
            [ onKey "Enter" (_ { shift = true }) pure (Actions.sendMessage draft)
            , A.onInput \text -> Actions.editDraft draft.id text
            ]
      ]
      [ H.text $ cardContent card ]
    ]
  , H.divS
    [ S.fontSize "0.75em"
    , S.textAlign "right"
    , S.fontStyle "italic"
    , S.opacity "0.5"
    ]
    [ ]
    [ let authorName = cardAuthorId card >>= flip Map.lookup userNames # fromMaybe "<anonymous>"
      in H.text $ (unwrap $ cardId card) <> " from " <> authorName ]
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

onKey :: forall act. String -> Opts { self :: Boolean, shift :: Boolean } -> act -> act -> A.Attribute act
onKey key mkOpts actNoop actDoIt =
  let opts = mkOpts { self: false, shift: false } in
  A.on "keydown" \event -> pure $
    case Wwg.toMaybeKeyboardEvent event of
      Nothing -> actNoop
      Just keyEvent -> do
        let
          keyOk = Wwg.key keyEvent == key
          selfOk = (opts.self `implies` _) $ Wwg.target keyEvent === Wwg.currentTarget keyEvent
          shiftOk = (opts.shift `implies` _) $ Wwg.shiftKey keyEvent
          ok = keyOk && selfOk && shiftOk
        if ok then actDoIt else actNoop

  where implies a b = not a || b
