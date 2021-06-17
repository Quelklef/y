module Y.Server.ServerConfig where

import Prelude

import Effect (Effect)
import Effect.Exception (throw)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))

import Y.Server.Util.Env (getEnv)

type ServerConfig =
  { sslConfig :: SslConfig
  }

data SslConfig
  = SslConfig_NoSsl
  | SslConfig_UseSsl { cert :: String, key :: String }

getServerConfig :: Effect ServerConfig
getServerConfig = do

  sslCert <- getEnv "Y_SSL_CERT" Just
  sslKey <- getEnv "Y_SSL_KEY" Just
  sslConfig <- case sslCert /\ sslKey of
    Just cert /\ Just key -> pure $ SslConfig_UseSsl { cert, key }
    Nothing /\ Nothing -> pure SslConfig_NoSsl
    _ -> throw $ "Expected either both or neither of Y_SSL_{CERT,KEY} to be set"

  pure { sslConfig }
