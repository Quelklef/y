module Client.View (view) where

import Prelude

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
    positions = cards # arrange
                            cardId
                            (cardDeps >>> Set.toUnfoldable)
                            (case _ of
                                Card_Message m -> m.time
                                Card_Draft d -> d.timeCreated)
                            (calcDims <<< viewCard convoState.userNames Vec2.origin)
    cardHtmls = cards # map \card -> let position = unsafePartial $ fromJust (Map.lookup (cardId card) positions)
                                     in viewCard convoState.userNames position card
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
    (List.toUnfoldable cardHtmls)

viewCard :: Map (Id "User") String -> Vec2 -> Card -> Html Action
viewCard userNames position card =
  H.divS
  [ S.position "absolute"
  , S.top $ (show (unwrap position).y) <> "px"
  , S.left $ (show (unwrap position).x) <> "px"
  , S.width "300px"
  , S.height "auto"
  , S.display "inline-block"
  ]
  [ A.onClick \model -> pure $ model { focused = Just (cardId card) }
  , guard (not $ isDraft card) $ onKey "Enter" defOpts pure Actions.createDraft
  ]
  [ H.divS
    [ ]
    [ ]
    [ H.textareaS
      [ ]
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
    [ ]
    [ ]
    [ let authorName = cardAuthorId card >>= flip Map.lookup userNames # fromMaybe "<anonymous>"
      in H.text authorName ]
  ]

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
