module Y.Client.Core where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.Map as Map
import Data.List as List
import Data.Newtype (wrap)

import Y.Shared.Util.Instant (Instant(..))
import Y.Shared.Id (Id)

type Message =
  { id :: Id "Message"
  , timeSent :: Instant
  , authorId :: Id "User"
  , convoId :: Id "Convo"
  , depIds :: Set (Id "Message")
  , content :: String
  }

type Model =
  { userId :: Id "User"
  , messages :: Set Message
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  }

mkInitialModel :: Model
mkInitialModel =
  { userId: wrap "y-user-testing-1"
  , messages: Set.fromFoldable
    [ { id: wrap "y-message-testing-1"
      , timeSent: Instant { milliseconds: 1.0 }
      , authorId: wrap "y-user-testing-1"
      , convoId: wrap "y-convo-testing-1"
      , depIds: Set.empty
      , content: "I am message #1"
      }
    ]
  , drafts: Set.singleton
      { id: wrap "y-message-testing-2"
      , depIds: Set.singleton $ wrap "y-message-testing-1"
      , timeCreated: Instant { milliseconds: 2.0 }
      , content: ""
      }
  , selectedIds: Set.empty
  , focusedId: Nothing
  }

type Draft =
  { id :: Id "Message"
  , depIds :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
