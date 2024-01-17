module Y.Client.Derived where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Data.Map (Map)
import Data.Map as Map
import Data.List (List)
import Data.List as List
import Data.Maybe (fromMaybe)
import Data.Foldable (class Foldable, foldl)
import Data.Lens (set, over)

import Y.Shared.Event (Event (..), EventPayload (..))
import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted

import Y.Client.Core (Derived, EventsAndDerived)

import Mation.Lenses (field)


appendEvent :: Event -> (EventsAndDerived -> EventsAndDerived)
appendEvent (Event event) { events, derived } =

  let
    alreadySeen = events # Sorted.map (\(Event ev) -> ev.id) # List.elem event.id
    isLatest = events # Sorted.unSorted # List.last # map (\(Event ev) -> ev.time) # map (_ < event.time) # fromMaybe true
  in
    -- if event is already seen, no need to do anything
    if alreadySeen then { events, derived }
    else
      let events' = Sorted.insert (Event event) events in
      { events: events'
      , derived:
          -- if appending event, do so incrementally
          if isLatest then patch (Event event) derived
          -- otherwise, have to recompute everything
          else recompute events'
      }

  where

  recompute :: forall f. Foldable f => f Event -> Derived
  recompute = foldl (flip patch) derived0
    where
      derived0 =
        { userNames: Map.empty
        , messages: Set.empty
        , unreadMessageIds: Set.empty
        , userIdToFirstEventTime: Map.empty
        }

  patch :: Event -> Derived -> Derived
  patch (Event event) = patch'firstEventTime >>> patch'eventSpecific
    where

    patch'firstEventTime :: Derived -> Derived
    patch'firstEventTime =
      let uid = case event.payload of
            EventPayload_SetName { userId } -> userId
            EventPayload_MessageSend { userId } -> userId
            EventPayload_MessageEdit { userId } -> userId
            EventPayload_MessageDelete { userId } -> userId
            EventPayload_MessageSetIsUnread { userId } -> userId
      in over (field @"userIdToFirstEventTime") (Map.insert uid event.time)

    patch'eventSpecific :: Derived -> Derived
    patch'eventSpecific =
      case event.payload of
        EventPayload_SetName pl ->
          over (field @"userNames") (Map.insert pl.userId pl.name)

        EventPayload_MessageSend pl ->
          over (field @"messages")
               (Set.insert  
                  { id: pl.messageId
                  , timeSent: pl.timeSent
                  , authorId: pl.userId
                  , depIds: pl.depIds
                  , content: pl.content
                  , deleted: false
                  })
          {-  FIXME
          >>> over (field @"unreadMessageIds")
                   (if pl.userId == model.userId
                    then identity
                    else Set.insert pl.messageId)
          -}

        EventPayload_MessageEdit pl ->
          over (field @"messages" <<< Set.map)
               (\msg -> if msg.id == pl.messageId
                        then msg # set (field @"content") pl.content
                        else msg)

        EventPayload_MessageDelete pl ->
          over (field @"messages" <<< Set.map)
               (\msg -> if msg.id == pl.messageId
                        then msg { content = "", deleted = true }
                        else msg)

        EventPayload_MessageSetIsUnread pl ->
          over (field @"unreadMessageIds")
               ((if pl.isUnread then Set.insert else Set.delete) pl.messageId)

