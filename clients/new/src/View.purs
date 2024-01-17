module Y.Client.View (view) where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Class (liftEffect)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.List (List)
import Data.List as List
import Data.Array as Array
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isNothing)
import Data.Int as Int
import Data.Number (infinity)
import Data.Tuple.Nested ((/\))
import Data.Foldable (fold, foldl, minimumBy, length, foldMap, all)
import Data.Newtype (unwrap)
import Data.Generic.Rep (class Generic)
import Data.Monoid (guard)
import Data.String.Common (trim) as String
import Data.Lazy as Lazy
import Effect.Unsafe (unsafePerformEffect)
import Control.Alt ((<|>))
import Partial.Unsafe (unsafePartial)
import Data.Argonaut.Encode (toJsonString)
import Data.Functor.Contravariant (cmap)
import Data.Lens (over, Iso', iso)
import Data.Lens.At (at)
import Data.Function (on)

import Mation (Html', DomEvent)
import Mation.Core.Refs (ReadWrite, Write, Modify)
import Mation.Core.Refs as Ref
import Mation.Lenses (field)
import Mation.Elems as E
import Mation.Props as P
import Mation.Styles as S

import Y.Shared.Instant (Instant, asMilliseconds, getNow)
import Y.Shared.Event (Event (..), EventPayload (..))
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted

import Y.Client.Core (Model, Message, EventsAndDerived, lEventsAndDerived)
import Y.Client.Derived as Derived
import Y.Client.Util.Vec2 (Vec2)
import Y.Client.Util.Vec2 as Vec2
import Y.Client.Util.Memoize (memoize)
import Y.Client.Util.Global (global)
import Y.Client.Action (Action (..))
import Y.Client.Actions as Actions
import Y.Client.Layout (layouts) as Layout
import Y.Client.Layouts.Core (invoke) as Layout
import Y.Client.CalcDims (calcDims)
import Y.Client.Colors as Colors


foreign import getTimestampPretty :: Effect String

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust m = unsafePartial $ fromJust m

view :: Model -> Html' (ReadWrite Model)
view model =
  E.div
  []
  [ E.div
    []
    [ model.derived.messages
        # Array.fromFoldable
        # Array.sortBy (compare `on` _.timeSent)
        # foldMap viewMessage
    ]
  , E.hr []
  , E.p
    []
    [ let
        replyingTo = model.selectedIds
        draftContent = model.drafts # Map.lookup replyingTo # fromMaybe ""
      in
        cmap
          (\(ref :: ReadWrite Model) ->
              { content:
                  ref
                  # Ref.focusWithLens (field @"drafts" <<< at replyingTo <<< isoMaybeString)
                  # Ref.downcast
              , sendMessage: sendMessage replyingTo ref
              })
          (viewSendMessageBox draftContent)
    ]
  ]

  where

  -- Not a real isomorphism. Oops! tee hee
  isoMaybeString :: Iso' (Maybe String) String
  isoMaybeString =
    iso
      (case _ of Nothing -> ""
                 Just s -> s)
      (case _ of "" -> Nothing
                 s -> Just s)

  viewMessage :: forall a. Message -> Html' a
  viewMessage message =
    E.div
    [ P.addStyles
      [ S.whiteSpace "pre-wrap"
      ]
    ]
    [ E.text $ "Message: " <> message.content
    ]

  sendMessage :: Set (Id "Message") -> ReadWrite Model -> String -> Effect Unit
  sendMessage replyingTo ref content = do
    now <- liftEffect getNow
    messageId <- liftEffect Id.new
    eventId <- liftEffect Id.new
    let event = Event
          { id: eventId
          , time: now
          , roomId: model.roomId
          , payload: EventPayload_MessageSend
            { messageId
            , depIds: replyingTo
            , timeSent: now
            , content
            , userId: model.userId
            }
          }
    ref # Ref.modify (over lEventsAndDerived (Derived.appendEvent event))

  getUserHue :: Id "User" -> Maybe Number
  getUserHue = \id -> Map.lookup id userIdToOrder # map (Colors.hueSeq seed)
    where
      seed = Id.format model.roomId
      userIdToOrder =
        model.derived.userIdToFirstEventTime
        # map asMilliseconds
        # flip Map.union (Map.singleton model.userId infinity)
        -- ^ If this client's user has sent no messages, assign an infinite time
        --   Note that 'flip Map.union' is right-biased
        # Map.keys
        # Set.toUnfoldable
        # List.sortBy (comparing $ flip Map.lookup model.derived.userIdToFirstEventTime)
        # List.toUnfoldable
        # Array.mapWithIndex (\idx userId -> Map.singleton userId idx)
        # foldl Map.union Map.empty



viewSendMessageBox :: String -> Html'
  { content :: Write String
  , sendMessage :: String -> Effect Unit
  }
viewSendMessageBox content =
  E.textarea
  [ P.onKeyup \(ev :: DomEvent) caps -> do
        -- Write textarea content to model
        content <- getTargetValue ev
        caps.content # Ref.write content
        -- Send message if enter pressed
        isSend ev >>= case _ of
          true -> do
            ev # preventDefault
            caps.sendMessage content
            caps.content # Ref.write ""  -- Clear input box (not working)
          false -> pure unit
  , P.onKeydown \ev _ -> do
        isSend ev >>= case _ of
          true -> preventDefault ev
          false -> pure unit
  , P.value content
  ]
  []

  where

  isSend ev = do
    keyInfo <- getKeyInfo ev
    pure $ keyInfo.key == "Enter" && (keyInfo.ctrl || keyInfo.shift)

foreign import getTargetValue :: DomEvent -> Effect String
foreign import getKeyInfo :: DomEvent -> Effect { key :: String, ctrl :: Boolean, shift :: Boolean }
foreign import preventDefault :: DomEvent -> Effect Unit
