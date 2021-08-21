module Y.Shared.Util.MonadJuggle where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

--import Text.Parsing.Parser (ParserT, fail) as Parsing
import Effect.Exception (throw) as Effect
import Effect.Class (liftEffect) as Effect
import Effect.Aff (Aff) as Aff

-- | Like MonadThrow, but without the `m -> e` functional dependency
class Monad m <= MonadJuggle e m where
  juggle :: forall a. e -> m a

-- | Generalized version of `Data.Either (note)`
note :: forall m a. MonadJuggle String m => String -> Maybe a -> m a
note s Nothing = juggle s
note _ (Just v) = pure v

fromEither :: forall m e a. MonadJuggle e m => Either e a -> m a
fromEither (Left e) = juggle e
fromEither (Right a) = pure a

-- instances --

--instance monadJuggle_String_ParserT :: Monad m => MonadJuggle String (Parsing.ParserT s m) where
--  juggle = Parsing.fail

instance monadJuggle_Either :: MonadJuggle e (Either e) where
  juggle = Left

instance monadJuggle_String_Aff :: MonadJuggle String Aff.Aff where
  juggle = Effect.throw >>> Effect.liftEffect
