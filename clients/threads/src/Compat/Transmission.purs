module Compat.Transmission where

import MasonPrelude

import Compat.Event (Event, fromEvent, toEvent)
import Compat.Id (convertId)

import Data.Argonaut.Encode (class EncodeJson) as Agt
import Data.Argonaut.Decode (class DecodeJson) as Agt
import Data.Argonaut.Encode.Generic (genericEncodeJson) as Agt
import Data.Argonaut.Decode.Generic (genericDecodeJson) as Agt

import Y.Shared.Id (Id)
import Y.Shared.Transmission as Trans

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


toToServer :: ToServer -> Trans.ToServer
toToServer = case _ of
  ToServer_Subscribe { convoId } ->
    Trans.ToServer_Subscribe { roomId: convertId convoId }
  ToServer_Pull { convoId } ->
    Trans.ToServer_Pull { roomId: convertId convoId }
  ToServer_Push { convoId, event } ->
    let
      roomId :: Id "Room"
      roomId = convertId convoId
    in
    Trans.ToServer_Push { roomId, event: toEvent roomId event }

toToClient :: Id "Room" -> ToClient -> Trans.ToClient
toToClient roomId (ToClient_Broadcast es) =
  Trans.ToClient_Broadcast (toEvent roomId <$> es)

fromToClient :: Trans.ToClient -> ToClient
fromToClient (Trans.ToClient_Broadcast es) =
  ToClient_Broadcast (fromEvent <$> es)

derive instance genericToServer :: Generic ToServer _
instance encodeJsonToServer :: Agt.EncodeJson ToServer where encodeJson = Agt.genericEncodeJson
instance decodeJsonToServer :: Agt.DecodeJson ToServer where decodeJson = Agt.genericDecodeJson

derive instance genericToClient :: Generic ToClient _
instance encodeJsonToClient :: Agt.EncodeJson ToClient where encodeJson = Agt.genericEncodeJson
instance decodeJsonToClient :: Agt.DecodeJson ToClient where decodeJson = Agt.genericDecodeJson
