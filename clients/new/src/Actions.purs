module Y.Client.Actions where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.List as List
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Int (floor)
import Data.Number as Math
import Data.Foldable (foldl, foldMap)
import Data.Monoid (power)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Random (random)
import Control.Monad.Reader.Class (ask)

import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Instant (getNow)
import Y.Shared.Instant as Instant
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Transmission as Transmission

import Y.Client.Core (Model)
import Y.Client.Action (Action(..), afterRender, unAction)
import Y.Client.WebSocket as Ws

import Data.Lens (over, set)
import Mation.Lenses (field)

noop :: Action
noop = Action pure

fromEvent :: Event -> Action
fromEvent = \(Event event) -> Action \model -> pure $
  let
    alreadySeen = model.events # Sorted.map (\(Event ev) -> ev.id) # List.elem event.id
    isLatest = model.events # Sorted.unSorted # List.last # map (\(Event ev) -> ev.time) # map (_ < event.time) # fromMaybe true
    model' = model { events = Sorted.insert (Event event) model.events }
  in
    -- if event is already seen, no need to do anythign
    if alreadySeen then model
    -- if appending event, do so incrementally
    else if isLatest then patch (Event event) model'
    -- otherwise, have to recompute everything
    else recompute model'

  where

  patch :: Event -> Model -> Model
  patch (Event event) = patch'firstEventTime >>> patch'eventSpecific
    where

    patch'firstEventTime :: Model -> Model
    patch'firstEventTime model =
      let uid = case event.payload of
            EventPayload_SetName { userId } -> userId
            EventPayload_MessageSend { userId } -> userId
            EventPayload_MessageEdit { userId } -> userId
            EventPayload_MessageDelete { userId } -> userId
            EventPayload_MessageSetIsUnread { userId } -> userId
      in model # over (field @"derived" <<< field @"userIdToFirstEventTime")
                      (Map.insert uid event.time)

    patch'eventSpecific :: Model -> Model
    patch'eventSpecific model =
      case event.payload of
        EventPayload_SetName pl ->
          model # over (field @"derived" <<< field @"userNames")
                       (Map.insert pl.userId pl.name)

        EventPayload_MessageSend pl ->
          model
            # over (field @"derived" <<< field @"messages")
                   (Set.insert  
                      { id: pl.messageId
                      , timeSent: pl.timeSent
                      , authorId: pl.userId
                      , depIds: pl.depIds
                      , content: pl.content
                      , deleted: false
                      })
            # over (field @"derived" <<< field @"unreadMessageIds")
                   (if pl.userId == model.userId then identity else Set.insert pl.messageId)

        EventPayload_MessageEdit pl ->
          model
            # over (field @"derived" <<< field @"messages" <<< Set.map)
                   (\msg -> if msg.id == pl.messageId
                            then msg # set (field @"content") pl.content
                            else msg)

        EventPayload_MessageDelete pl ->
          model
            # over (field @"derived" <<< field @"messages" <<< Set.map)
                   (\msg -> if msg.id == pl.messageId
                            then msg { content = "", deleted = true }
                            else msg)

        EventPayload_MessageSetIsUnread pl ->
          model
            # over (field @"derived" <<< field @"unreadMessageIds")
                   ((if pl.isUnread then Set.insert else Set.delete) pl.messageId)

  recompute :: Model -> Model
  recompute model = model.events # foldl (flip patch) model0
    where
    model0 = model
      { derived =
        { userNames: Map.empty
        , messages: Set.empty
        , unreadMessageIds: Set.empty
        , userIdToFirstEventTime: Map.empty
        }
      }

sendEvent :: Event -> Action
sendEvent event = Action \model -> do
  -- Eagerly record event locally
  model' <- unAction (fromEvent event) model

  -- Send event to server
  let roomId = model.roomId
  let transmission = Transmission.ToServer_Push { roomId, event }
  wsClient <- _.wsClient <$> ask
  liftEffect $ wsClient # Ws.transmit transmission

  pure $ model'

setFocused :: Id "Message" -> Action
setFocused id = Action \model -> pure $ model { focusedId = Just id }

setSelected :: Id "Message" -> Boolean -> Action
setSelected id to = Action \model ->
  pure $ model { selectedIds = (if to then Set.insert else Set.delete) id model.selectedIds }

toggleContextMenuFor :: Id "Message" -> Action
toggleContextMenuFor targetId = Action \model ->
  pure $ model { openContextMenuMessageId = case model.openContextMenuMessageId of
    Nothing -> Just targetId
    Just openId -> if openId == targetId then Nothing else Just targetId
  }

-- TODO The `focus*` functions should be reimplemented:
-- We retrieve the elements via `getElementById()`. This is bad because it splits the
-- logic into two independent parts: one, assigning the element id, which is in the
-- view, and two, actually calling `focus()`, which is here. These two parts are
-- entirely separate within the code base, far apart, and can be edited and/or removed
-- independent of each other other, without complaint from the compiler, all despite
-- consisting of a single conceptual unit. Yuck.

focusDraftTextarea :: Id "Message" -> Effect Unit
focusDraftTextarea draftId = do
  let textareaId = "textarea-for-" <> (Id.format draftId)
  focusElementById textareaId

focusCard :: Id "Message" -> Effect Unit
focusCard cardId = do
  let cardDomId = "card-" <> (Id.format cardId)
  focusElementById cardDomId

foreign import setTimeout0 :: forall a. Effect a -> Effect Unit
foreign import focusElementById :: String -> Effect Unit

sendMessage ::
  { replyingTo :: Set (Id "Message")
  , content :: String
  } -> Action
sendMessage { replyingTo, content } =
  Action \model -> do
    now <- liftEffect getNow
    messageId <- liftEffect Id.new
    eventId <- liftEffect Id.new
    let event = Event
          { id: eventId
          , time: now
          , roomId: model.roomId
          , payload: EventPayload_MessageSend
            { messageId
            , timeSent: now
            , userId: model.userId
            , depIds: replyingTo
            , content
            }
          }
    model' <- unAction (sendEvent event) model

    -- ↓ Remove the draft from the model
    pure $ model' # over (field @"drafts") (Map.delete replyingTo)

setName :: String -> Action
setName newName = Action \model -> do
  let roomId = model.roomId
  let userId = model.userId

  now <- liftEffect getNow
  eventId <- liftEffect Id.new
  let event = Event { id: eventId, time: now, roomId, payload: EventPayload_SetName { userId, name: newName } }
  model' <- unAction (sendEvent event) model

  pure $ model' { nicknameInputValue = Nothing }

setIsUnread :: Id "Message" -> Boolean -> Action
setIsUnread messageId isUnread = Action \model -> do
  now <- liftEffect getNow
  eventId <- liftEffect Id.new

  let event = Event
        { id: eventId
        , time: now
        , roomId: model.roomId
        , payload: EventPayload_MessageSetIsUnread
          { userId: model.userId
          , messageId: messageId
          , isUnread
          }
        }

  model' <- unAction (sendEvent event) model
  pure model'

deleteMessage :: Id "Message" -> Action
deleteMessage messageId = Action \model -> do
  now <- liftEffect getNow
  eventId <- liftEffect Id.new

  let event = Event
        { id: eventId
        , time: now
        , roomId: model.roomId
        , payload: EventPayload_MessageDelete
          { userId: model.userId
          , messageId: messageId
          }
        }

  model' <- unAction (sendEvent event) model
  pure model'

appendManyMessages :: Action
appendManyMessages = appendRandomCard `power` 12
  where
  appendRandomCard = Action \model -> do
    now1 <- liftEffect getNow
    let now2 = Instant.fromMilliseconds $ Instant.asMilliseconds now1 + 1.0
    userId <- liftEffect Id.new
    eventId1 <- liftEffect Id.new
    eventId2 <- liftEffect Id.new
    messageId <- liftEffect Id.new

    depCount <- liftEffect random <#> \x -> floor ((x `Math.pow` 3.0) * 4.0) + 1
    shuffledMsgs <- liftEffect $ model.derived.messages # Set.toUnfoldable # shuffle
    let depIds = shuffledMsgs # Array.slice 0 depCount # map _.id # Set.fromFoldable

    contentLength <- liftEffect random <#> \x -> floor (x * 50.0) + 5
    let content = "x" `power` contentLength

    let events =
          [ Event
            { id: eventId1
            , time: now1
            , roomId: model.roomId
            , payload: EventPayload_MessageSend
              { messageId
              , timeSent: now1
              , depIds: depIds
              , content: content
              , userId
              }
            }
          , Event
            { id: eventId2
            , time: now2
            , roomId: model.roomId
            , payload: EventPayload_MessageSetIsUnread
              { messageId: messageId
              , userId: model.userId
              , isUnread: false
              }
            }
          ]

    model' <- unAction (foldMap fromEvent events) model
    pure $ model'

foreign import shuffle :: forall a. Array a -> Effect (Array a)


-- Download a file to the user
download :: Effect { name :: String, content :: String } -> Action
download get = Action $ \m -> liftEffect do
  { name, content } <- get
  download_f name content
  pure m

foreign import download_f :: String -> String -> Effect Unit
