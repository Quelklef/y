module Y.Server.ServerConfig where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Y.Server.Util.Env as Env

type ServerConfig =
  { sslConfig :: SslConfig
  , dbConnectionString :: String
  }

data SslConfig
  = SslConfig_NoSsl
  | SslConfig_UseSsl { cert :: String, key :: String }

getServerConfig :: Effect ServerConfig
getServerConfig = do

  sslCert <- Env.getEnv "Y_SSL_CERT" Just
  sslKey <- Env.getEnv "Y_SSL_KEY" Just
  sslConfig <- case sslCert /\ sslKey of
    Just cert /\ Just key -> pure $ SslConfig_UseSsl { cert, key }
    Nothing /\ Nothing -> pure SslConfig_NoSsl
    _ -> throw $ "Expected either both or neither of Y_SSL_{CERT,KEY} to be set"

  dbConnectionString <- Env.getEnv "Y_DB_CONNECTION_STRING" (Just # Env.required)

  pure { sslConfig, dbConnectionString }

  where

  parseBool :: String -> Maybe Boolean
  parseBool = case _ of
    "0" -> Just false
    "1" -> Just true
    "false" -> Just false
    "true" -> Just true
    "False" -> Just false
    "True" -> Just true
    _ -> Nothing
