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

editDraft :: Id "Message" -> String -> Action
editDraft draftId text model = do
  pure $ model { drafts = model.drafts # Set.map (\draft -> if draft.id == draftId then draft { content = text } else draft) }
