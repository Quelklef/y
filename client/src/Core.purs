module Y.Client.Core where

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.Map (Map)
import Data.Map as Map

import Y.Shared.Util.Instant (Instant)
import Y.Client.Util.Sorted (Sorted)
import Y.Client.Util.Sorted as Sorted
import Y.Shared.Id (Id)
import Y.Shared.Event (Event)
import Y.Shared.Transmission as Transmission
import Y.Client.Arrange as Arrange
import Y.Client.WebSocket as Ws

type Y_Ws_Client = Ws.Client Transmission.ToServer Transmission.ToClient

type Model =
  { userId :: Id "User"
  , roomId :: Id "Room"
  , events :: Sorted Event
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  , arrangementAlgorithmKey :: String
  , nicknameInputValue :: Maybe String
  , screenDims :: { width :: Number, height :: Number }
  , openContextMenuMessageId :: Maybe (Id "Message")

  -- v Redundant information derived from other model data
  --   Kept in the model for efficieny's sake
  --   Code that changes the model is expected to keep this
  --   information up-to-date (sorry).
  , userNames :: Map (Id "User") String
  , messages :: Set Message
  , unreadMessageIds :: Set (Id "Message")
  , userIdToFirstEventTime :: Map (Id "User") Instant
  }

type Message =
  { id :: Id "Message"
  , timeSent :: Instant
  , authorId :: Id "User"
  , depIds :: Set (Id "Message")
  , content :: String
  , deleted :: Boolean
  }

mkInitialModel :: Id "User" -> Id "Room" -> Model
mkInitialModel userId roomId =
  { userId: userId
  , roomId: roomId
  , events: Sorted.sort List.Nil
  , drafts: Set.empty
  , selectedIds: Set.empty
  , focusedId: Nothing
  , arrangementAlgorithmKey: Arrange.defaultAlgoKey
  , nicknameInputValue: Nothing
  , screenDims: { width: 0.0, height: 0.0 }  -- will be set by a subscription
  , openContextMenuMessageId: Nothing

  , userNames: Map.empty
  , messages: Set.empty
  , unreadMessageIds: Set.empty
  , userIdToFirstEventTime: Map.empty
  }

type Draft =
  { id :: Id "Message"
  , depIds :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
