module Compat.Message where

import Data.Set (Set)

import Y.Shared.Id (Id)
import Y.Shared.Instant (Instant)

type Message =
  { id :: Id "Message"
  , timeSent :: Instant
  , authorId :: Id "User"
  , convoId :: Id "Convo"
  , depIds :: Set (Id "Message")
  , content :: String
  , deleted :: Boolean
  }
