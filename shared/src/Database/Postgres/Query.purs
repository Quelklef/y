module Database.Postgres.Query where

import Prelude

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Exception (Error, throw) as Ex
import Control.Promise (Promise, toAffE)
import Control.Monad.Error.Class (class MonadError, throwError, catchError)
import Data.Bifunctor (lmap, rmap)
import Data.Newtype (un)
import Data.Traversable (traverse)
import Data.Either (Either(..))

import Database.Postgres.ToPg (class ToPg, toPg)
import Database.Postgres.FromPg (class FromPg, fromPg, ParseErr)
import Database.Postgres.Types (PgExpr(..), Tup0, tup0)
import Database.Postgres.Internal.ParseComposite (parseComposite)

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

-- TODO: better types
data PgErr
  = PgErr_ParamErr String    -- failed to parse parameters
  | PgErr_ExecErr Ex.Error   -- error when executing sql
  | PgErr_ResultErr ParseErr -- failed to parse results

toThrow :: forall m a. MonadAff m => Aff (Either PgErr a) -> m a
toThrow aff = liftAff $ aff >>= \a -> liftEffect $ case a of
  Left (PgErr_ParamErr e) -> Ex.throw $ show e
  Left (PgErr_ExecErr e) -> throwError e
  Left (PgErr_ResultErr e) -> Ex.throw $ show e
  Right val -> pure val

-- | Perform a query
query ::
  forall p m r. MonadAff m => ToPg p => FromPg r =>
  String -> p -> Database -> m (Either PgErr (Array r))
query sql params db = liftAff $
  case parseParams (toPg params) of
    Left e -> pure (Left $ PgErr_ParamErr e)
    Right paramExprs -> do
      eitherResultExprs <- catchIntoEither $ toAffE $ query_f { db, sql, params: paramExprs }
      pure $ do
        resultExprs <- eitherResultExprs # lmap PgErr_ExecErr
        results <- traverse fromPg resultExprs # lmap PgErr_ResultErr
        pure results

  where

  catchIntoEither :: forall m' e a. MonadError e m' => m' a -> m' (Either e a)
  catchIntoEither m = catchError (Right <$> m) (pure <<< Left)

  parseParams :: PgExpr -> Either String (Array PgExpr)
  parseParams expr =
    expr
    # parseComposite { open: "(", delim: ",", close: ")" }
    # lmap (\err ->
      "Failed to parse PostgreSQL parameters.\n"
      <> "This likely means you provided parameters as a value (:: a) instead of a row (:: Tup a).\n"
      <> "Parameter expression: " <> un PgExpr expr <> "\n"
      <> "Underlying parse error: " <> show err)

-- | Like `query`, but errors are thrown in `Aff`
queryThrow ::
  forall p m r. MonadAff m => ToPg p => FromPg r =>
  String -> p -> Database -> m (Array r)
queryThrow sql params db = toThrow $ query sql params db

-- | Like `query`, but no query parameters
query_ ::
  forall m r. MonadAff m => FromPg r =>
  String -> Database -> m (Either PgErr (Array r))
query_ sql db = query sql tup0 db

-- | Like `query_`, but errors are thrown in `Aff`
queryThrow_ ::
  forall m r. MonadAff m => FromPg r =>
  String -> Database -> m (Array r)
queryThrow_ sql db = toThrow $ query_ sql db

-- | Like `query`, but no return value
exec ::
  forall p m. MonadAff m => ToPg p =>
  String -> p -> Database -> m (Either PgErr Unit)
exec sql params db = rmap (\(_ :: Array Tup0) -> unit) <$> query sql params db

-- | Like `exec`, but errors are thrown in `Aff`
execThrow ::
  forall p m. MonadAff m => ToPg p =>
  String -> p -> Database -> m Unit
execThrow sql params db = toThrow $ exec sql params db

-- | Like `exec`, but no query parameters or return value
exec_ ::
  forall m. MonadAff m =>
  String -> Database -> m (Either PgErr Unit)
exec_ sql db = exec sql tup0 db

-- | Like `exec_`, but errors are thrown in `Aff`
execThrow_ ::
  forall m. MonadAff m =>
  String -> Database -> m Unit
execThrow_ sql db = toThrow $ exec_ sql db

atomically :: forall a. Aff a -> Database -> Aff Unit
atomically act db = do
  catchError
    (do
      db # execThrow_ "BEGIN"
      void act
      db # execThrow_ "COMMIT")
    (\err -> do
      db # execThrow_ "ROLLBACK"
      throwError err)
