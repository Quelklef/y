module Y.Client.Core where

import Data.Set (Set)
import Data.Set as Set
import Data.Maybe (Maybe(..))
import Data.List as List

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)
import Y.Shared.Convo (Convo)
import Y.Client.Arrange as Arrange

type Model =
  { userId :: Id "User"
  , convo :: Convo
  , drafts :: Set Draft
  , selectedIds :: Set (Id "Message")
  , focusedId :: Maybe (Id "Message")
  , arrangementAlgorithmKey :: String
  , nicknameInputValue :: Maybe String
  }

mkInitialModel :: Id "User" -> Id "Convo" -> Model
mkInitialModel userId convoId =
  { userId: userId
  , convo:
    { id: convoId
    , events: List.Nil
    }
  , drafts: Set.empty
  , selectedIds: Set.empty
  , focusedId: Nothing
  , arrangementAlgorithmKey: Arrange.defaultAlgoKey
  , nicknameInputValue: Nothing
  }

type Draft =
  { id :: Id "Message"
  , depIds :: Set (Id "Message")
  , timeCreated :: Instant
  , content :: String
  }
