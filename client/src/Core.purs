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
  , content :: String
  , isDraft :: Boolean
  }

type Model =
  { messages :: Set Message
  }

mkInitialModel :: Model
mkInitialModel =
  { messages: Set.fromFoldable
    [ { id: wrap "y-message-testing-1"
      , content: "I am message #1"
      , isDraft: false
      }
    , { id: wrap "y-message-testing-2"
      , content: ""
      , isDraft: true
      }
    ]
  }
