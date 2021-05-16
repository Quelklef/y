module Client.Core where

import Effect (Effect)

import Shared.Id (Id)
import Shared.Convo (Convo)

-- assumed to be sorted by time
type Model =
  { userId :: Id "User"
  , convo :: Convo
  }

type Msg = Model -> Effect Model
