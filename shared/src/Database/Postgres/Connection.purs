module Database.Postgres.Connection
  ( Connection
  , open
  ) where

import Prelude

import Effect (Effect)
import Control.Promise (Promise, toAffE)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)

foreign import data Connection :: Type
foreign import open_f :: String -> Effect (Promise Connection)

open :: String -> Aff Connection
open connectionString = liftAff <<< toAffE $ open_f connectionString
