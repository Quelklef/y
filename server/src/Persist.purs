module Y.Server.Persist where

import Prelude

import Effect.Aff (Aff)
import Effect.Console as Console
import Effect.Class (liftEffect)
import Control.Monad.Error.Class (throwError)
import Data.Tuple.Nested ((/\))
import Data.Either (Either(..), note)
import Data.Array as Array

import Y.Shared.Util.Sorted (Sorted)
import Y.Shared.Util.Sorted as Sorted
import Y.Shared.Event (Event(..), EventPayload(..))
import Y.Shared.Id (Id)
import Y.Shared.ToFromPostgres (PgRetrievedVal(..)) as Pg

import Y.Server.ServerConfig (ServerConfig)
import Y.Server.Postgres (class FromPgRow, class ToPgRow, Database, Tup(..), all, atomically, fromPgRow, new, run, run_) as Pg

open :: ServerConfig -> Aff Pg.Database
open config = Pg.new config.dbConnectionString

insertEvent :: Event -> Pg.Database -> Aff Unit
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

  insertEvent_fromPayload :: forall r. Pg.ToPgRow (Pg.Tup r) => String -> String -> r -> Aff Unit
  insertEvent_fromPayload payloadKind payloadInsertSql payloadRow =
    db # Pg.run
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

unRetrievedEvent :: RetrievedEvent -> Event
unRetrievedEvent (RetrievedEvent event) = event

instance instance_FromPgRow_RetrievedEvent :: Pg.FromPgRow (Either String) RetrievedEvent where
  fromPgRow row = do

    let eventStuff = Array.slice 0 4 row
    payloadStuff <-
      Array.index row 4
      # note "Missing event payload"
      >>= case _ of
        Pg.PgRow ar -> pure ar
        _ -> throwError "Expected array"

    Pg.Tup (id /\ payloadKind /\ time /\ roomId) <- Pg.fromPgRow eventStuff

    payload <- case payloadKind of
      "SetName" -> do
        Pg.Tup (userId /\ name) <- Pg.fromPgRow payloadStuff
        pure $ EventPayload_SetName { userId, name }

      "MessageSend" -> do
        Pg.Tup (userId /\ messageId /\ depIds /\ timeSent /\ content) <- Pg.fromPgRow payloadStuff
        pure $ EventPayload_MessageSend { userId, messageId, depIds, timeSent, content }

      "MessageEdit" -> do
        Pg.Tup (userId /\ messageId /\ content) <- Pg.fromPgRow payloadStuff
        pure $ EventPayload_MessageEdit { userId, messageId, content }

      "MessageDelete" -> do
        Pg.Tup (userId /\ messageId) <- Pg.fromPgRow payloadStuff
        pure $ EventPayload_MessageDelete { userId, messageId }

      "MessageSetIsUnread" -> do
        Pg.Tup (userId /\ messageId /\ isUnread) <- Pg.fromPgRow payloadStuff
        pure $ EventPayload_MessageSetIsUnread { userId, messageId, isUnread }

      _ -> do
        throwError $ "Unrecognized payload kind: " <> payloadKind

    pure $ RetrievedEvent $ Event { id, time, payload, roomId }

retrieveEvents :: Id "Room" -> Pg.Database -> Aff (Sorted Array Event)
retrieveEvents = \roomId db -> do
  (maybeRetrievedEvents :: Either String (Array RetrievedEvent)) <- db # Pg.all """
    SELECT 

      id
    , payloadKind
    , time
    , roomId

    , CASE
        WHEN payloadKind = 'SetName'
        THEN ( SELECT to_json((userId, name)) FROM EventPayload_SetName WHERE pk = payloadPk )
          -- ^ to_json used to deal with the fact that the underlying js <-> pg lib
          --   can't parse tuples :(

        WHEN payloadKind = 'MessageSend'
        THEN ( SELECT to_json((userId, messageId, depIds, timeSent, content)) FROM EventPayload_MessageSend WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageEdit'
        THEN ( SELECT to_json((userId, messageId, content)) FROM EventPayload_MessageEdit WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageDelete'
        THEN ( SELECT to_json((userId, messageId)) FROM EventPayload_MessageDelete WHERE pk = payloadPk )

        WHEN payloadKind = 'MessageSetIsUnread'
        THEN ( SELECT to_json((userId, messageId, isUnread)) FROM EventPayload_MessageSetIsUnread WHERE pk = payloadPk )
      END

    FROM Event
    WHERE roomId = $1
    ORDER BY time ASC;
  """ (Pg.Tup roomId)

  let maybeEvents = maybeRetrievedEvents # (map <<< map) unRetrievedEvent

  case maybeEvents of
    Left err -> do
      liftEffect $ Console.warn $ "Failure when pulling from database: " <> err
      pure $ Sorted.empty
    Right events -> do
      pure $ Sorted.fromAsc events

migrate :: ServerConfig -> Pg.Database -> Aff Unit
migrate config db =

  -- TODO: instead of using IF NOT EXISTS, support a migrations folder
  db # Pg.atomically do
    db # Pg.run_ """
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
