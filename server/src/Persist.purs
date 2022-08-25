module Y.Server.Persist where

import Prelude

import Effect.Aff (Aff)
import Data.Tuple.Nested ((/\))
import Data.Newtype (class Newtype, un)
import Data.Either (Either(..))
import Data.Bifunctor (lmap)

import Database.Postgres.ToPg (class InnerTup) as Pg  -- TODO: leaking implementation details =(
import Database.Postgres.FromPg (class FromPg, fromPg, mkImpl) as Pg
import Database.Postgres.Types (PgExpr, Tup(..)) as Pg
import Database.Postgres.Connection (Connection, open) as Pg
import Database.Postgres.Query as Pq

import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Id (Id)

import Y.Server.ServerConfig (ServerConfig)

open :: ServerConfig -> Aff Pg.Connection
open config = Pg.open config.dbConnectionString

insertEvent :: Event -> Pg.Connection -> Aff Unit
insertEvent (Event event) db = case event.payload of

  EventPayload_SetName pl -> insertEvent_fromPayload "SetName"
    "INSERT INTO EventPayload_SetName (userId, name) VALUES ($5, $6)"
    ( pl.userId /\ pl.name )

  EventPayload_MessageSend pl -> insertEvent_fromPayload "MessageSend"
    "INSERT INTO EventPayload_MessageSend (userId, messageId, depIds, timeSent, content) VALUES ($5, $6, $7, $8, $9)"
    ( pl.userId /\ pl.messageId /\ pl.depIds /\ pl.timeSent /\ pl.content )

  EventPayload_MessageEdit pl -> insertEvent_fromPayload "MessageEdit"
    "INSERT INTO EventPayload_MessageEdit (userId, messageId, content) VALUES ($5, $6, $7)"
    ( pl.userId /\ pl.messageId /\ pl.content )

  EventPayload_MessageDelete pl -> insertEvent_fromPayload "MessageDelete"
    "INSERT INTO EventPayload_MessageDelete (userId, messageId) VALUES ($5, $6)"
    ( pl.userId /\ pl.messageId )

  EventPayload_MessageSetIsUnread pl -> insertEvent_fromPayload "MessageSetIsUnread"
    "INSERT INTO EventPayload_MessageSetIsUnread (userId, messageId, isUnread) VALUES ($5, $6, $7)"
    ( pl.userId /\ pl.messageId /\ pl.isUnread )

  where

  insertEvent_fromPayload :: forall r. Pg.InnerTup r => String -> String -> r -> Aff Unit
  insertEvent_fromPayload payloadKind payloadInsertSql payloadRow =
    db # Pq.execThrow
      ("""
      WITH returned AS (
        """ <> payloadInsertSql <> """
        RETURNING pk
      )
      INSERT INTO Event (id, payloadPk, payloadKind, time, roomId)
        SELECT $1, pk, $2, $3, $4 FROM returned
      """)
      ( Pg.Tup $ event.id /\ payloadKind /\ event.time /\ event.roomId /\ payloadRow )

newtype RetrievedEvent = RetrievedEvent Event

derive instance newtype_RetrievedEvent :: Newtype RetrievedEvent _

instance fromPgRow_RetrievedEvent :: Pg.FromPg RetrievedEvent where
  impl = Pg.mkImpl
    \(Pg.Tup (id /\ payloadKind /\ time /\ roomId /\ (payloadStuff :: Pg.PgExpr))) -> do

      payload <- case payloadKind of
        "SetName" -> lmap show do
          Pg.Tup (userId /\ name) <- Pg.fromPg payloadStuff
          pure $ EventPayload_SetName { userId, name }

        "MessageSend" -> lmap show do
          Pg.Tup (userId /\ messageId /\ depIds /\ timeSent /\ content) <- Pg.fromPg payloadStuff
          pure $ EventPayload_MessageSend { userId, messageId, depIds, timeSent, content }

        "MessageEdit" -> lmap show do
          Pg.Tup (userId /\ messageId /\ content) <- Pg.fromPg payloadStuff
          pure $ EventPayload_MessageEdit { userId, messageId, content }

        "MessageDelete" -> lmap show do
          Pg.Tup (userId /\ messageId) <- Pg.fromPg payloadStuff
          pure $ EventPayload_MessageDelete { userId, messageId }

        "MessageSetIsUnread" -> lmap show do
          Pg.Tup (userId /\ messageId /\ isUnread) <- Pg.fromPg payloadStuff
          pure $ EventPayload_MessageSetIsUnread { userId, messageId, isUnread }

        _ -> do
          Left $ "Unrecognized payload kind: " <> payloadKind

      pure $ RetrievedEvent $ Event { id, time, payload, roomId }

retrieveEvents :: Id "Room" -> Pg.Connection -> Aff (Sorted Array Event)
retrieveEvents = \roomId db -> do
  -- TODO: don't error entire array if only one is malformatted
  (events :: Array RetrievedEvent) <- db # Pq.queryThrow """
    SELECT 

      id
    , payloadKind
    , time
    , roomId

    , CASE
        WHEN payloadKind = 'SetName'
        THEN ( SELECT row(userId, name) FROM EventPayload_SetName WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageSend'
        THEN ( SELECT row(userId, messageId, depIds, timeSent, content) FROM EventPayload_MessageSend WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageEdit'
        THEN ( SELECT row(userId, messageId, content) FROM EventPayload_MessageEdit WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageDelete'
        THEN ( SELECT row(userId, messageId) FROM EventPayload_MessageDelete WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageSetIsUnread'
        THEN ( SELECT row(userId, messageId, isUnread) FROM EventPayload_MessageSetIsUnread WHERE pk = payloadPk )
      END

    FROM Event
    WHERE roomId = $1
    ORDER BY time ASC;
  """ (Pg.Tup roomId)
  pure $ Sorted.fromAsc (events # map (un RetrievedEvent))

migrate :: ServerConfig -> Pg.Connection -> Aff Unit
migrate _config db =

  -- TODO: instead of using IF NOT EXISTS, support a migrations folder
  db # Pq.atomically do
    db # Pq.execThrow_ """
      CREATE TABLE IF NOT EXISTS Event
        ( id          TEXT        PRIMARY KEY
        , payloadPk   INT         NOT NULL
            -- ^ Reference to one of the EventPayload_* tables
            --   Note that while events have 'id's, event payloads have 'pk's
            --   This is to differentiate between identifiers which exist
            --   only within the database (pks) from those which also exist
            --   outside of the database (ids).
        , payloadKind TEXT        NOT NULL
            -- ^  Designates which EventPayload table payloadId references
            CHECK(payloadKind in ('SetName', 'MessageSend', 'MessageEdit', 'MessageDelete', 'MessageSetIsUnread'))
        , time        TIMESTAMPTZ NOT NULL
        , roomId      TEXT        NOT NULL
        );

      CREATE INDEX IF NOT EXISTS idx__Event__roomId ON Event (roomId);
      CREATE INDEX IF NOT EXISTS idx__Event__time ON Event (time);
      CREATE INDEX IF NOT EXISTS idx__Event__roomId_x_time ON Event (roomId, time);

      CREATE TABLE IF NOT EXISTS EventPayload_SetName
        ( pk     SERIAL PRIMARY KEY
        , userId TEXT   NOT NULL
        , name   TEXT   NOT NULL
        );

      CREATE TABLE IF NOT EXISTS EventPayload_MessageSend
        ( pk        SERIAL      PRIMARY KEY
        , userId    TEXT        NOT NULL
        , messageId TEXT        NOT NULL
        , depIds    TEXT[]      NOT NULL
        , timeSent  TIMESTAMPTZ NOT NULL
        , content   TEXT        NOT NULL
        );

      CREATE TABLE IF NOT EXISTS EventPayload_MessageEdit
        ( pk        SERIAL PRIMARY KEY
        , userId    TEXT   NOT NULL
        , messageId TEXT   NOT NULL
        , content   TEXT   NOT NULL
        );

      CREATE TABLE IF NOT EXISTS EventPayload_MessageDelete
        ( pk        SERIAL PRIMARY KEY
        , userId    TEXT   NOT NULL
        , messageId TEXT   NOT NULL
        );

      CREATE TABLE IF NOT EXISTS EventPayload_MessageSetIsUnread
        ( pk        SERIAL PRIMARY KEY
        , userId    TEXT   NOT NULL
        , messageId TEXT   NOT NULL
        , isUnread  BOOL   NOT NULL
        );

      CREATE INDEX IF NOT EXISTS idx__EventPayload_SetName__pk            ON EventPayload_SetName (pk);
      CREATE INDEX IF NOT EXISTS idx__EventPayload_MessageSend__pk        ON EventPayload_MessageSend (pk);
      CREATE INDEX IF NOT EXISTS idx__EventPayload_MessageEdit__pk        ON EventPayload_MessageEdit (pk);
      CREATE INDEX IF NOT EXISTS idx__EventPayload_MessageDelete__pk      ON EventPayload_MessageDelete (pk);
      CREATE INDEX IF NOT EXISTS idx__EventPayload_MessageSetIsUnread__pk ON EventPayload_MessageSetIsUnread (pk);
    """
