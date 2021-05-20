module Y.Client.Action where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Data.List (List)
import Control.Monad (class Monad, (>>=))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)

import Y.Shared.Transmission (Transmission)
import Y.Shared.Convo (Event)

import Y.Client.WebSocket as Ws
import Y.Client.Core (Model)

newtype Action = Action (Model -> ActionMonad Model)

instance semigroupAction :: Semigroup Action where
  append (Action a1) (Action a2) = Action (a1 >=> a2)

instance monoidAction :: Monoid Action where
  mempty = Action pure

newtype ActionMonad a = ActionMonad (ReaderT ActionAnswer Effect a)

type ActionAnswer =
  { wsClient :: Ws.Client Transmission (List Event)
  }

runAction :: ActionAnswer -> Action -> (Model -> Effect Model)
runAction answer (Action action) model =
  case action model of ActionMonad mr -> runReaderT mr answer

unActionMonad :: forall a. ActionMonad a -> ReaderT ActionAnswer Effect a
unActionMonad (ActionMonad r) = r

instance bindActionMonad :: Bind ActionMonad where
  bind a' f = ActionMonad do
    a <- unActionMonad a'
    r <- unActionMonad (f a)
    pure r

instance applyActionMonad :: Apply ActionMonad where
  apply (ActionMonad f) (ActionMonad x) = ActionMonad (f <*> x)

instance functorActionMonad :: Functor ActionMonad where
  map f (ActionMonad x) = ActionMonad (f <$> x)

instance applicativeActionMonad :: Applicative ActionMonad where
  pure = ActionMonad <<< pure

instance monadActionMonad :: Monad ActionMonad

instance monadEffectActionMonad :: MonadEffect ActionMonad where
  liftEffect ef = ActionMonad (lift ef)

instance monadReaderActionMonad :: MonadAsk ActionAnswer ActionMonad where
  ask = ActionMonad ask
