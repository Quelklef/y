module Client.Core where

import Effect (Effect)
import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List (List)
import Data.List as List

import Shared.Util.Instant (Instant)
import Shared.Id (Id)
import Shared.Convo (Event, Convo)

type Model =
  { userId :: Id "User"
  , convo :: Convo
  , drafts :: Set Draft
  , selected :: Set (Id "Message")
  , focused :: Maybe (Id "Message")
  }

mkInitialModel :: Id "User" -> Id "Convo" -> Model
mkInitialModel userId convoId =
  { userId: userId
  , convo:
    { id: convoId
    , events: List.Nil
    }
  , drafts: Set.empty
  , selected: Set.empty
  , focused: Nothing
  }

type Draft =
  { id :: Id "Message"
  , deps :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
