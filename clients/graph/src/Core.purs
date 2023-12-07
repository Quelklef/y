module Y.Client.Core where

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.Map (Map)
import Data.Map as Map

import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Instant (Instant)
import Y.Shared.Id (Id)
import Y.Shared.Event (Event)
import Y.Shared.Transmission as Transmission
import Y.Client.Layout as Layout
import Y.Client.WebSocket as Ws

type Y_Ws_Client = Ws.Client Transmission.ToServer Transmission.ToClient

type Model =
  { userId :: Id "User"
  , roomId :: Id "Room"
  , events :: Sorted List Event
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  , nicknameInputValue :: Maybe String
  , openContextMenuMessageId :: Maybe (Id "Message")

  -- v Redundant information derived from other model data
  --   Kept in the model for efficieny's sake
  --   Code that changes the model is expected to keep this
  --   information up-to-date (sorry).
  , derived ::
    { userNames :: Map (Id "User") String
    , messages :: Set Message
    , unreadMessageIds :: Set (Id "Message")
    , userIdToFirstEventTime :: Map (Id "User") Instant
    }
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
  , events: Sorted.empty
  , drafts: Set.empty
  , selectedIds: Set.empty
  , focusedId: Nothing
  , nicknameInputValue: Nothing
  , openContextMenuMessageId: Nothing

  , derived:
    { userNames: Map.empty
    , messages: Set.empty
    , unreadMessageIds: Set.empty
    , userIdToFirstEventTime: Map.empty
    }
  }

type Draft =
  { id :: Id "Message"
  , depIds :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
