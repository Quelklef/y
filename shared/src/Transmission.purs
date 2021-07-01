module Y.Shared.Transmission where

import Data.Generic.Rep (class Generic)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Id (Id)
import Y.Shared.Event (Event)

data ToServer

  -- | Initialization message
  = ToServer_Hello
    { userId :: Id "User"  -- identity of client
    }

  -- | Request realtime events for given room
  | ToServer_Subscribe
    { roomId :: Id "Room"
    }

  -- | Request existing events for given room
  | ToServer_Pull
    { roomId :: Id "Room"
    }

  -- | Push an event to a room
  | ToServer_Push
    { roomId :: Id "Room"
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
