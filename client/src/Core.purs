module Y.Client.Core where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List as List
import Data.Newtype (wrap)

import Y.Shared.Util.Instant (Instant(..))
import Y.Shared.Id (Id)
import Y.Shared.Convo (Event, Convo, EventPayload(..))

type Model =
  { userId :: Id "User"
  , convo :: Convo
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  }

mkInitialModel :: Model
mkInitialModel =
  { userId: wrap "y-user-testing-1"
  , convo:
    { id: wrap "y-convo-testing-1"
    , events: List.fromFoldable
        [ { id: wrap "y-event-testing-1"
          , time: Instant { milliseconds: 1.0 }
          , payload: EventPayload_MessageSend
            { convoId: wrap "y-convo-testing-1"
            , message:
              { id: wrap "y-message-testing-1"
              , timeSent: Instant { milliseconds: 1.0 }
              , authorId: wrap "y-user-testing-1"
              , convoId: wrap "y-convo-testing-1"
              , depIds: Set.empty
              , content: "I am message #1"
              }
            }
          }
        ]
    }
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
