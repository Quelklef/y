module Y.Client.Core where

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

import Y.Shared.Util.Instant (Instant)
import Y.Client.Util.Sorted (Sorted)
import Y.Client.Util.Sorted as Sorted
import Y.Shared.Id (Id)
import Y.Shared.Event (Event)
import Y.Shared.Message (Message)
import Y.Client.Arrange as Arrange

type Model =
  { userId :: Id "User"
  , convoId :: Id "Convo"
  , events :: Sorted Event
  -- v "_r" = "redundant", computed from events
  -- v TODO: having computed info in the model directly seems not ideal
  -- v       it requires any caller which updates .events to also worry
  -- v       about recomputing the other stuff
  -- v       (Currently ok since .events is only changed in 1 location)
  , userNames_r :: Map (Id "User") String
  , messages_r :: Set Message
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  , arrangementAlgorithmKey :: String
  , nicknameInputValue :: Maybe String
  , screenDims :: { width :: Number, height :: Number }
  }

mkInitialModel :: Id "User" -> Id "Convo" -> Model
mkInitialModel userId convoId =
  { userId: userId
  , convoId: convoId
  , events: Sorted.sort List.Nil
  , userNames_r: Map.empty
  , messages_r: Set.empty
  , drafts: Set.empty
  , selectedIds: Set.empty
  , focusedId: Nothing
  , arrangementAlgorithmKey: Arrange.defaultAlgoKey
  , nicknameInputValue: Nothing
  , screenDims: { width: 0.0, height: 0.0 }  -- will be set by a subscription
  }

type Draft =
  { id :: Id "Message"
  , depIds :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
