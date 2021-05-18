module Y.Client.Actions where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Monad.Reader.Class (ask)

import Y.Shared.Util.Instant (getNow)
import Y.Shared.Id (Id, newId)
import Y.Shared.Convo (EventPayload(..))
import Y.Shared.Transmission (Transmission(..))

import Y.Client.Core (Model, Draft)
import Y.Client.Action (Action)
import Y.Client.WebSocket as Ws

noop :: Action
noop = pure

createDraft :: Action
createDraft model = do
  (mid :: Id "Message") <- liftEffect newId
  now <- liftEffect getNow

  let (draft :: Draft) =
        { id: mid
        , deps: model.selected <> (map Set.singleton model.focused # fromMaybe Set.empty)
        , content: ""
        , timeCreated: now
        }

  let (newModel :: Model) = model
        { focused = Just mid
        , selected = (Set.empty :: Set (Id "Message"))
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
  let textareaId = "textarea-for-" <> (unwrap draftId)
  setTimeout0 do
    focusElementById textareaId

foreign import setTimeout0 :: forall a. Effect a -> Effect Unit
foreign import focusElementById :: String -> Effect Unit

editDraft :: Id "Message" -> String -> Action
editDraft draftId text model = do
  pure $ model { drafts = model.drafts # Set.map (\draft -> if draft.id == draftId then draft { content = text } else draft) }

sendMessage :: Draft -> Action
sendMessage draft model = do
  now <- liftEffect getNow

  let convoId = model.convo.id
  let authorId = model.userId
  let message =
        { id: draft.id
        , time: now
        , authorId
        , convoId
        , deps: draft.deps
        , content: draft.content
        }

  eventId <- liftEffect newId
  let event = { id: eventId, time: now, payload: EventPayload_MessageSend { convoId, message } }
  let transmission = Transmission_Push { convoId, event }
  wsClient <- _.wsClient <$> ask
  liftEffect $ wsClient # Ws.transmit transmission

  pure $ model { drafts = model.drafts # Set.filter (\d -> d.id /= draft.id) }
