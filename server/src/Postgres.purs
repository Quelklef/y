module Y.Server.Postgres where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Promise (Promise, toAffE)
import Control.Monad.Error.Class (throwError, catchError)
import Data.Bifunctor (lmap)
import Data.Newtype (un)
import Data.Traversable (traverse)

import Y.Shared.Util.MonadJuggle (class MonadJuggle, fromEither)
import Y.Shared.Pg.ToPg (class ToPg, toPg)
import Y.Shared.Pg.FromPg (class FromPg)
import Y.Shared.Pg.FromPg (fromPg) as Pg
import Y.Shared.Pg.Types (PgExpr(..), Tup0, tup0)
import Y.Shared.Pg.Internal.ParseComposite (parseComposite)

foreign import data Database :: Type
foreign import new_f :: String -> Effect (Promise Database)
foreign import query_f ::
  { db :: Database
  , sql :: String
  , params :: Array PgExpr
  } -> Effect (Promise (Array PgExpr))  -- (pg expr)[]

toAff' :: forall m a. MonadAff m => Effect (Promise a) -> m a
toAff' = toAffE >>> liftAff

new :: String -> Aff Database
new connectionString = toAff' $ new_f connectionString

type PgErr = String  -- TODO

-- | Perform a query
query ::
  forall p m r.
  MonadJuggle PgErr m => MonadAff m => ToPg p => FromPg r =>
  String -> p -> Database -> m (Array r)
query sql params db = do
  paramExprs <- parseParams $ toPg params
  exprs <- query_f { db, sql, params: paramExprs } # toAff'
  vals <- traverse Pg.fromPg exprs
  pure vals

  where

  parseParams :: MonadJuggle PgErr m => PgExpr -> m (Array PgExpr)
  parseParams expr =
    expr
    # parseComposite { open: "(", delim: ",", close: ")" }
    # lmap (\err ->
      "Failed to parse PostgreSQL parameters.\n"
      <> "This likely means you provided parameters as a value (:: a) instead of a row (:: Tup a).\n"
      <> "Parameter expression: " <> un PgExpr expr <> "\n"
      <> "Underlying parse error: " <> err)
    # fromEither

-- | Like `query`, but no query parameters
query_ ::
  forall m r.
  MonadJuggle PgErr m => MonadAff m => FromPg r =>
  String -> Database -> m (Array r)
query_ sql db = query sql tup0 db

-- | Like `query`, but no return value
exec ::
  forall p m.
  MonadJuggle PgErr m => MonadAff m => ToPg p =>
  String -> p -> Database -> m Unit
exec sql params db = void (query sql params db :: m (Array Tup0))

-- | Like `query`, but no query parameters or return value
exec_ ::
  forall m.
  MonadJuggle PgErr m => MonadAff m =>
  String -> Database -> m Unit
exec_ sql db = exec sql tup0 db

atomically :: forall a. Aff a -> Database -> Aff Unit
atomically act db = do
  catchError
    (do
      db # exec_ "BEGIN"
      void act
      db # exec_ "COMMIT")
    (\err -> do
      db # exec_ "ROLLBACK"
      throwError err)
