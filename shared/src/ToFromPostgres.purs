module Y.Shared.ToFromPostgres where

-- Provides the ToPg and FromPg classes
--
-- Ideally, this module would be in the server codebase only rather than
-- the shared codebase.
-- However, we will want to define instances for these classes on some
-- shared datatypes; due to orphan instances being restricted, this forces
-- us to either use newtypes or to move the class definitions into the
-- shared codebase; I found the latter option to be the lesser evil.

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Data.Traversable (traverse)
import Data.Foldable (intercalate)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Array as Array
import Data.Number as Number
import Data.Int as Int

----------
-- ToPg --

class ToPg a where
  toPg :: a -> String
  -- ^ TODO: would be nice to include type in there, so that
  --         an int becomes "'<number>'::int4" and a number
  --         becomes "'<number>'::double precision", but it
  --         seems like the postgres lib I'm using doesn't support
  --         that syntax :(
  -- ^ TODO: might be good to 'newtype PgIn = PgIn String'

instance toPg_String :: ToPg String where
  toPg = identity

instance toPg_Boolean :: ToPg Boolean where
  toPg = show

instance toPg_Number :: ToPg Number where
  toPg = show

instance toPg_Int :: ToPg Int where
  toPg = show

instance toPg_Maybe :: ToPg a => ToPg (Maybe a) where
  toPg = case _ of
    Nothing -> "null"
    Just v -> toPg v

instance toPg_Array :: ToPg a => ToPg (Array a) where
  toPg ar = ar # map toPg # intercalate "," # (\s -> "{" <> s <> "}")

instance toPg_Set :: (Ord a, ToPg a) => ToPg (Set a) where
  toPg = Set.toUnfoldable >>> Array.sort >>> toPg

------------
-- FromPg --

-- Value returned by underlying js <-> pg code
data PgRetrievedVal
  = PgNull
  | PgArray (Array PgRetrievedVal)
  | PgRow (Array PgRetrievedVal)
  | PgOther String

class FromPg :: forall k. (k -> Type) -> k -> Constraint
class FromPg m a where
  fromPg :: PgRetrievedVal -> m a

stringy :: forall m. MonadThrow String m => PgRetrievedVal -> m String
stringy = case _ of
  PgNull -> throwError "Expected string value, not NULL"
  PgArray _ -> throwError "Expected string value, not array"
  PgRow _ -> throwError "Expected string value, not row"
  PgOther s -> pure s

instance fromPg_String :: MonadThrow String m => FromPg m String where
  fromPg = stringy

instance fromPg_Boolean :: MonadThrow String m => FromPg m Boolean where
  fromPg = stringy >=> case _ of
    "t" -> pure true
    "f" -> pure false
    s -> throwError $ "Failed to decode bool; expected 't' or 'f', not: " <> show s

instance fromPg_Number :: MonadThrow String m => FromPg m Number where
  fromPg = stringy >=> (Number.fromString >>> case _ of
    Nothing -> throwError "Expected number"
    Just v -> pure v)

instance fromPg_Int :: MonadThrow String m => FromPg m Int where
  fromPg = (fromPg :: PgRetrievedVal -> m Number) >=> (Int.fromNumber >>> case _ of
    Nothing -> throwError "Expected integral number"
    Just v -> pure v)

instance fromPg_Maybe :: (MonadThrow String m, FromPg m a) => FromPg m (Maybe a) where
  fromPg = case _ of
    PgNull -> pure Nothing
    PgArray a -> Just <$> fromPg (PgArray a)
    PgOther s -> Just <$> fromPg (PgOther s)
    PgRow r -> Just <$> fromPg (PgRow r)

instance fromPg_Array :: (MonadThrow String m, FromPg m a) => FromPg m (Array a) where
  fromPg = case _ of
    PgNull -> throwError "Expected array value, not NULL"
    PgOther s -> throwError $ "Expected array value, not: " <> show s
    PgRow _ -> throwError $ "Expected array value, not row"
    PgArray vs -> traverse fromPg vs

instance fromPg_Set :: (Ord a, MonadThrow String m, FromPg m a) => FromPg m (Set a) where
  fromPg = (fromPg :: PgRetrievedVal -> m (Array a)) >>> map Set.fromFoldable
