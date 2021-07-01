module Y.Server.Util.Env (getEnv, withDefault, required) where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Data.Maybe (Maybe(..), fromMaybe)

foreign import getEnv_f
  :: (forall a. a -> Maybe a) -> (forall a. Maybe a)
  -> String -> Effect (Maybe String)

getEnv :: forall a. String -> (Maybe String -> Maybe a) -> Effect a
getEnv varname parse = do
  maybeStr <- getEnv_f Just Nothing varname
  let maybeVal = parse maybeStr
  case maybeVal of
    Nothing -> throw $ "Invalid format for env var " <> varname
    Just val -> pure val

withDefault :: forall a. a -> (String -> Maybe a) -> (Maybe String -> Maybe a)
withDefault default parse = map parse >>> fromMaybe (Just default)

required :: forall a. (String -> Maybe a) -> (Maybe String -> Maybe a)
required parse = case _ of
  Just s -> parse s
  Nothing -> Nothing
