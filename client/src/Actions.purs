module Client.Actions where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..), fromMaybe)
import Effect.Class (liftEffect)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Class (ask)

import Shared.Util.Instant (getNow)
import Shared.Id (Id, newId)
import Shared.Convo (Event, EventPayload(..))
import Shared.Transmission (Transmission(..))
import Shared.Util.Instant (Instant)

import Client.Core (Draft)
import Client.Action (Action, ActionMonad, ActionAnswer)
import Client.WebSocket as Ws

createDraft :: Action
createDraft model = do
  (mid :: Id "Message") <- liftEffect newId
  now <- liftEffect getNow
  let draft =
        { id: mid
        , deps: model.selected <> (map Set.singleton model.focused # fromMaybe Set.empty)
        , content: ""
        , timeCreated: now
        }
  pure $ model
    { focused = Just mid
    , selected = (Set.empty :: Set (Id "Message"))
    , drafts = model.drafts <> Set.singleton draft
    }

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
