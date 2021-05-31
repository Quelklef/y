module Y.Client.Actions where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Map as Map
import Data.Foldable (foldl)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader.Class (ask)

import Y.Shared.Util.Instant (getNow)
import Y.Shared.Id (Id)
import Y.Shared.Id as Id
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Transmission (Transmission(..))

import Y.Client.Util.Sorted as Sorted
import Y.Client.Core (Model, Draft)
import Y.Client.Action (Action(..), unAction)
import Y.Client.WebSocket as Ws

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
  patch (Event event) model =
    case event.payload of
      EventPayload_SetName pl ->
        model { userNames_r = model.userNames_r # Map.insert pl.userId pl.name }

      EventPayload_MessageSend pl ->
        model { messages_r = model.messages_r <> Set.singleton pl.message
              , unreadMessageIds_r =
                  if pl.message.authorId == model.userId
                  then model.unreadMessageIds_r
                  else model.unreadMessageIds_r # Set.insert pl.message.id
              }

      EventPayload_SetReadState pl ->
        model { unreadMessageIds_r =
                  model.unreadMessageIds_r
                  # (if not pl.readState then Set.insert else Set.delete) pl.messageId
              }

  recompute :: Model -> Model
  recompute model = model.events # foldl (flip patch) model0
    where
    model0 = model
      { userNames_r = Map.empty
      , messages_r = Set.empty
      , unreadMessageIds_r = Set.empty
      }

sendEvent :: Event -> Action
sendEvent event = Action \model -> do
  -- Eagerly record event locally
  model' <- unAction (fromEvent event) model

  -- Send event to server
  let convoId = model.convoId
  let transmission = Transmission_Push { convoId, event }
  wsClient <- _.wsClient <$> ask
  liftEffect $ wsClient # Ws.transmit transmission

  pure $ model'

setFocused :: Id "Message" -> Action
setFocused id = Action \model -> pure $ model { focusedId = Just id }

setSelected :: Id "Message" -> Boolean -> Action
setSelected id to = Action \model ->
  pure $ model { selectedIds = (if to then Set.insert else Set.delete) id model.selectedIds }

createDraft :: Action
createDraft = Action \model -> do
  (mid :: Id "Message") <- liftEffect Id.new
  now <- liftEffect getNow

  let (draft :: Draft) =
        { id: mid
        , depIds: model.selectedIds <> (map Set.singleton model.focusedId # fromMaybe Set.empty)
        , content: ""
        , timeCreated: now
        }

  let (newModel :: Model) = model
        { focusedId = Just mid
        , selectedIds = (Set.empty :: Set (Id "Message"))
        , drafts = model.drafts <> Set.singleton draft
        }

  liftEffect $ focusDraftTextareaAfterRender draft.id

  pure newModel

focusDraftTextareaAfterRender :: Id "Message" -> Effect Unit
focusDraftTextareaAfterRender draftId = do
  -- Set the focus to the textarea once the draft has been rendered
  -- The implementation here is basically a giant hack:
  -- 1) We use setTimeout(, 0) to perform the .focus() after the render
  --    Instead, we should use Elmish's afterRender. But I didn't want to
  --    go through the effort to make Cmd available to the program, since:
  --    a) Mason says Cmd needs reworking anyway;
  --    b) Even if I did, this implementation would still be a huge
  --       hack ANYWAY, because:
  -- 2) We retrieve the textare element via .getElementById("textarea-for-" + draftId).
  --    This is bad because it splits the logic into two independent parts: one,
  --    assigning the element id, which is in the view, and two, actually calling .focus(),
  --    which is here. These two parts are entirely separate within the code base, far
  --    apart, and can be edited and/or removed independent of each other other, without
  --    complaint from the compiler, all despite consisting of a single conceptual unit.
  --    Yuck.
  -- This function should be reimplemented.
  let textareaId = "textarea-for-" <> (Id.format draftId)
  setTimeout0 do
    focusElementById textareaId

foreign import setTimeout0 :: forall a. Effect a -> Effect Unit
foreign import focusElementById :: String -> Effect Unit

editDraft :: Id "Message" -> String -> Action
editDraft draftId text = Action \model -> do
  pure $ model { drafts = model.drafts # Set.map (\draft -> if draft.id == draftId then draft { content = text } else draft) }

sendMessage :: Draft -> Action
sendMessage draft = Action \model -> do
  now <- liftEffect getNow

  let convoId = model.convoId
  let authorId = model.userId
  let message =
        { id: draft.id
        , timeSent: now
        , authorId
        , convoId
        , depIds: draft.depIds
        , content: draft.content
        }

  eventId <- liftEffect Id.new
  let event = Event { id: eventId, time: now, payload: EventPayload_MessageSend { convoId, message } }
  model' <- unAction (sendEvent event) model

  pure $ model' { drafts = model.drafts # Set.filter (\d -> d.id /= draft.id) }

setName :: String -> Action
setName newName = Action \model -> do
  let convoId = model.convoId
  let userId = model.userId

  now <- liftEffect getNow
  eventId <- liftEffect Id.new
  let event = Event { id: eventId, time: now, payload: EventPayload_SetName { convoId, userId, name: newName } }
  model' <- unAction (sendEvent event) model

  pure $ model' { nicknameInputValue = Nothing }

setReadState :: Id "Message" -> Boolean -> Action
setReadState messageId newReadState = Action \model -> do
  now <- liftEffect getNow
  eventId <- liftEffect Id.new

  let event = Event
        { id: eventId
        , time: now
        , payload: EventPayload_SetReadState
          { convoId: model.convoId
          , userId: model.userId
          , messageId: messageId
          , readState: newReadState
          }
        }

  model' <- unAction (sendEvent event) model
  pure model'
