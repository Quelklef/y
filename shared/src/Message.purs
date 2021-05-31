module Y.Shared.Message where

import Data.Set (Set)

import Y.Shared.Util.Instant (Instant)
import Y.Shared.Id (Id)

type Message =
  { id :: Id "Message"
  , timeSent :: Instant
  , authorId :: Id "User"
  , convoId :: Id "Convo"
  , depIds :: Set (Id "Message")
  , content :: String
  }
