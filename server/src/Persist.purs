module Y.Server.Persist where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Console as Console
import Effect.Class (liftEffect)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Array as Array
import Data.Set (Set)
import Data.Traversable (traverse)

import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Instant (Instant)
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Id (Id)

import Y.Server.ServerConfig (ServerConfig)
import Y.Server.Postgres as Postgres

open :: ServerConfig -> Aff Postgres.Database
open config = Postgres.new config.dbConnectionString

insertEvent :: Event -> Postgres.Database -> Aff Unit
insertEvent (Event event) db = case event.payload of
  EventPayload_SetName pl -> db # Postgres.run
    """
      INSERT INTO events (id, time, roomId, kind, SetName_userId, SetName_name)
      VALUES ($1, $2, $3, $4, $5, $6);
    """
    ( event.id /\ event.time /\ event.roomId /\ "SetName" /\ pl.userId /\ pl.name )

  EventPayload_MessageSend pl -> db # Postgres.run
    """
      INSERT INTO events (id, time, roomId, kind, MessageSend_userId, MessageSend_messageId, MessageSend_depIds, MessageSend_timeSent, MessageSend_content)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9);
    """
    ( event.id /\ event.time /\ event.roomId /\ "MessageSend" /\ pl.userId /\ pl.messageId /\ pl.depIds /\ pl.timeSent /\ pl.content )

  EventPayload_MessageEdit pl -> db # Postgres.run
    """
      INSERT INTO events (id, time, roomId, kind, MessageEdit_userId, MessageEdit_messageId, MessageEdit_content)
      VALUES ($1, $2, $3, $4, $5, $6, $7);
    """
    ( event.id /\ event.time /\ event.roomId /\ "MessageEdit" /\ pl.userId /\ pl.messageId /\ pl.content )

  EventPayload_MessageDelete pl -> db # Postgres.run
    """
      INSERT INTO events (id, time, roomId, kind, MessageDelete_userId, MessageDelete_messageId)
      VALUES ($1, $2, $3, $4, $5, $6);
    """
    ( event.id /\ event.time /\ event.roomId /\ "MessageDelete" /\ pl.userId /\ pl.messageId )

  EventPayload_MessageSetIsUnread pl -> db # Postgres.run
    """
      INSERT INTO events (id, time, roomId, kind, MessageSetIsUnread_userId, MessageSetIsUnread_messageId, MessageSetIsUnread_isUnread)
      VALUES ($1, $2, $3, $4, $5, $6, $7);
    """
    ( event.id /\ event.time /\ event.roomId /\ "MessageSetIsUnread" /\ pl.userId /\ pl.messageId /\ pl.isUnread )

type EventRow =
  (  Id "Event" /\ Instant /\ Id "Room" /\ String
  /\ Maybe (Id "User") /\ Maybe String
  /\ Maybe (Id "User") /\ Maybe (Id "Message") /\ Maybe (Set (Id "Message")) /\ Maybe Instant /\ Maybe String
  /\ Maybe (Id "User") /\ Maybe (Id "Message") /\ Maybe String
  /\ Maybe (Id "User") /\ Maybe (Id "Message")
  /\ Maybe (Id "User") /\ Maybe (Id "Message") /\ Maybe Boolean
  )

retrieveEvents :: Id "Room" -> Postgres.Database -> Aff (Sorted Array Event)
retrieveEvents = \roomId db -> do
  (maybeRows :: Either String (Array EventRow)) <- db # Postgres.all """
    SELECT 

      id
    , time
    , roomId
    , kind

    , SetName_userId
    , SetName_name

    , MessageSend_userId
    , MessageSend_messageId
    , MessageSend_depIds
    , MessageSend_timeSent
    , MessageSend_content

    , MessageEdit_userId
    , MessageEdit_messageId
    , MessageEdit_content

    , MessageDelete_userId
    , MessageDelete_messageId

    , MessageSetIsUnread_userId
    , MessageSetIsUnread_messageId
    , MessageSetIsUnread_isUnread

    FROM events
    WHERE roomId = $1
    ORDER BY time ASC;
  """ roomId

  case maybeRows of
    Left err -> do
      liftEffect $ Console.warn $ "Failure when pulling from database: " <> err
      pure $ Sorted.empty
    Right rows -> do
      events <- rows # traverse (rowToEvent >>> liftEffect) # map Array.catMaybes
      pure $ Sorted.fromAsc events

rowToEvent :: EventRow -> Effect (Maybe Event)
rowToEvent row = case row of
  ( id /\ time /\ roomId /\ _ ) -> do
    maybePayload <- rowToEventPayload row
    let maybeEvent = maybePayload # map \payload -> Event { id, time, roomId, payload }
    pure maybeEvent

rowToEventPayload :: EventRow -> Effect (Maybe EventPayload)
rowToEventPayload = case _ of

  ( _ /\ _ /\ _ /\ "SetName"
  /\ Just userId /\ Just name
  /\ Nothing /\ Nothing /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  ) -> pure <<< Just $ EventPayload_SetName { userId, name }

  ( _ /\ _ /\ _ /\ "MessageSend"
  /\ Nothing /\ Nothing
  /\ Just userId /\ Just messageId /\ Just depIds /\ Just timeSent /\ Just content
  /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  ) -> pure <<< Just $ EventPayload_MessageSend { userId, messageId, depIds, timeSent, content }

  ( _ /\ _ /\ _ /\ "MessageEdit"
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing /\ Nothing /\ Nothing
  /\ Just userId /\ Just messageId /\ Just content
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  ) -> pure <<< Just $ EventPayload_MessageEdit { userId, messageId, content }

  ( _ /\ _ /\ _ /\ "MessageDelete"
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  /\ Just userId /\ Just messageId
  /\ Nothing /\ Nothing /\ Nothing
  ) -> pure <<< Just $ EventPayload_MessageDelete { userId, messageId }

  ( _ /\ _ /\ _ /\ "MessageSetIsUnread"
  /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing /\ Nothing
  /\ Nothing /\ Nothing
  /\ Just userId /\ Just messageId /\ Just isUnread
  ) -> pure <<< Just $ EventPayload_MessageSetIsUnread { userId, messageId, isUnread }

  _ -> do
    Console.warn "Failed to decode database row to event"
    pure Nothing

migrate :: ServerConfig -> Postgres.Database -> Aff Unit
migrate config db =

  -- TODO: instead of using IF NOT EXISTS, support a migrations folder
  db # Postgres.atomically do
    db # Postgres.run_ """
      CREATE TABLE IF NOT EXISTS events
        ( id     TEXT        NOT NULL
        , time   TIMESTAMPTZ NOT NULL
        , roomId TEXT        NOT NULL
        , kind   TEXT        NOT NULL
            CHECK(kind in ('SetName', 'MessageSend', 'MessageEdit', 'MessageDelete', 'MessageSetIsUnread'))

        , SetName_userId TEXT                CHECK( (SetName_userId               IS NOT NULL) = (kind = 'SetName'           ) )
        , SetName_name   TEXT                CHECK( (SetName_name                 IS NOT NULL) = (kind = 'SetName'           ) )

        , MessageSend_userId    TEXT         CHECK( (MessageSend_userId           IS NOT NULL) = (kind = 'MessageSend'       ) )
        , MessageSend_messageId TEXT         CHECK( (MessageSend_messageId        IS NOT NULL) = (kind = 'MessageSend'       ) )
        , MessageSend_depIds    TEXT[]       CHECK( (MessageSend_depIds           IS NOT NULL) = (kind = 'MessageSend'       ) )
        , MessageSend_timeSent  TIMESTAMPTZ  CHECK( (MessageSend_timeSent         IS NOT NULL) = (kind = 'MessageSend'       ) )
        , MessageSend_content   TEXT         CHECK( (MessageSend_content          IS NOT NULL) = (kind = 'MessageSend'       ) )

        , MessageEdit_userId    TEXT         CHECK( (MessageEdit_userId           IS NOT NULL) = (kind = 'MessageEdit'       ) )
        , MessageEdit_messageId TEXT         CHECK( (MessageEdit_messageId        IS NOT NULL) = (kind = 'MessageEdit'       ) )
        , MessageEdit_content   TEXT         CHECK( (MessageEdit_content          IS NOT NULL) = (kind = 'MessageEdit'       ) )

        , MessageDelete_userId    TEXT       CHECK( (MessageDelete_userId         IS NOT NULL) = (kind = 'MessageDelete'     ) )
        , MessageDelete_messageId TEXT       CHECK( (MessageDelete_messageId      IS NOT NULL) = (kind = 'MessageDelete'     ) )

        , MessageSetIsUnread_userId    TEXT  CHECK( (MessageSetIsUnread_userId    IS NOT NULL) = (kind = 'MessageSetIsUnread') )
        , MessageSetIsUnread_messageId TEXT  CHECK( (MessageSetIsUnread_messageId IS NOT NULL) = (kind = 'MessageSetIsUnread') )
        , MessageSetIsUnread_isUnread  BOOL  CHECK( (MessageSetIsUnread_isUnread  IS NOT NULL) = (kind = 'MessageSetIsUnread') )
        );
    """
    db # Postgres.run_ "CREATE INDEX IF NOT EXISTS events_roomId_idx ON events (roomId);"
    db # Postgres.run_ "CREATE INDEX IF NOT EXISTS events_time_idx ON events (time);"
    db # Postgres.run_ "CREATE INDEX IF NOT EXISTS events_roomId_time_idx ON events (roomId, time);"
