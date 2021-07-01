module Y.Server.Postgres where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise (Promise, toAffE)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Array as Array
import Control.Monad.Error.Class (class MonadThrow, throwError, catchError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)

import Y.Shared.ToFromPostgres (class ToPg, toPg, PgRetrievedVal(..), class FromPg, fromPg)

class ToPgRow a where
  toPgRow :: a -> Array String

class FromPgRow :: forall k. (k -> Type) -> k -> Constraint
class FromPgRow m a where
  fromPgRow :: Array PgRetrievedVal -> m a

instance toPgRow_rec :: (ToPg a, ToPg b, ToPgRow r) => ToPgRow (a /\ (b /\ r)) where
  toPgRow (a /\ (b /\ r)) = [toPg a, toPg b] <> toPgRow r
else instance toPgRow_base :: (ToPg a, ToPg b) => ToPgRow (a /\ b) where
  toPgRow (a /\ b) = [toPg a, toPg b]
else instance toPgRow_one :: ToPg a => ToPgRow a where
  toPgRow a = [toPg a]

instance fromPgRow_rec :: (MonadThrow String m, FromPg m a, FromPg m b, FromPgRow m r) => FromPgRow m (a /\ (b /\ r)) where
  fromPgRow row =
    (do
      { head: a, tail: rest } <- Array.uncons row
      { head: b, tail: rest } <- Array.uncons rest
      pure $ a /\ b /\ rest)
    # case _ of
      Just (a /\ b /\ rest) -> (\x y z -> x /\ y /\ z) <$> fromPg a <*> fromPg b <*> fromPgRow rest
      Nothing -> throwError "Wrong number of results"
else instance fromPgRow_base :: (MonadThrow String m, FromPg m a, FromPg m b) => FromPgRow m (a /\ b) where
  fromPgRow = case _ of
    [a, b] -> (/\) <$> fromPg a <*> fromPg b
    _ -> throwError "Wrong number of results"
else instance fromPgRow_one :: (MonadThrow String m, FromPg m a) => FromPgRow m a where
  fromPgRow = case _ of
    [a] -> fromPg a
    _ -> throwError "Wrong number of results"

foreign import data Database :: Type
foreign import new_f :: String -> Effect (Promise Database)
foreign import query_f ::
  { nullVal :: PgRetrievedVal
  , arrayVal :: Array (PgRetrievedVal) -> PgRetrievedVal
  , otherVal :: String -> PgRetrievedVal
  , caseMaybeOf :: forall a r. Maybe a -> r -> (a -> r) -> r
  } ->
  Database -> String -> Maybe (Array String) -> Effect (Promise (Array (Array PgRetrievedVal)))

query :: Database -> String -> Maybe (Array String) -> Effect (Promise (Array (Array PgRetrievedVal)))
query = query_f
  { nullVal: PgNull
  , arrayVal: PgArray
  , otherVal: PgOther
  , caseMaybeOf: \maybe onNothing onJust -> maybe # map onJust # fromMaybe onNothing
  }

new :: String -> Aff Database
new connectionString = toAffE $ new_f connectionString

all :: forall v m r. Applicative m => ToPgRow v => FromPgRow m r => String -> v -> Database -> Aff (m (Array r))
all sql vals db = (toAffE $ query db sql (Just $ toPgRow vals)) # map (traverse fromPgRow)

all_ :: forall m r. Applicative m => FromPgRow m r => String -> Database -> Aff (m (Array r))
all_ sql db = (toAffE $ query db sql Nothing) # map (traverse fromPgRow)

one :: forall v m r. MonadThrow String m => ToPgRow v => FromPgRow m r => String -> v -> Database -> Aff (m r)
one sql vals db = all sql vals db # map (_ >>= case _ of
  [v] -> pure v
  r -> throwError $ "Expected exactly one value from database, not " <> show (Array.length r))

-- TODO: one_

run :: forall v. ToPgRow v => String -> v -> Database -> Aff Unit
run sql vals db = (toAffE $ query db sql (Just $ toPgRow vals)) # void

run_ :: String -> Database -> Aff Unit
run_ sql db = (toAffE $ query db sql Nothing) # void

atomically :: forall a. Aff a -> Database -> Aff Unit
atomically act db = do
  catchError
    (do
      db # run_ "BEGIN"
      _ <- act
      db # run_ "COMMIT")
    (\err -> do
      db # run_ "ROLLBACK"
      throwError err)
