module Y.Shared.Transmission where

import Data.Generic.Rep (class Generic)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Id (Id)
import Y.Shared.Event (Event)

data ToServer

  = ToServer_Subscribe
    { userId :: Id "User"  -- identity of subscribing client
    , convoId :: Id "Convo"
    }

  | ToServer_Pull
    { convoId :: Id "Convo"
    }

  | ToServer_Push
    { convoId :: Id "Convo"
    , event :: Event
    }

data ToClient

  = ToClient_Broadcast (Array Event)

derive instance genericToServer :: Generic ToServer _
instance encodeJsonToServer :: Agt.EncodeJson ToServer where encodeJson = Agt.genericEncodeJson
instance decodeJsonToServer :: Agt.DecodeJson ToServer where decodeJson = Agt.genericDecodeJson

derive instance genericToClient :: Generic ToClient _
instance encodeJsonToClient :: Agt.EncodeJson ToClient where encodeJson = Agt.genericEncodeJson
instance decodeJsonToClient :: Agt.DecodeJson ToClient where decodeJson = Agt.genericDecodeJson
