module Y.Client.Action where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)

import Y.Client.Core (Model, Y_Ws_Client)

newtype Action = Action (Model -> ActionMonad Model)

-- This unfortunate instance exists to satisfy 'Producer.Produce Action Action'
-- WANT: fix this, either upstream or here.
instance Eq Action where
  eq _ _ = false

unAction :: Action -> Model -> ActionMonad Model
unAction (Action a) = a

instance semigroupAction :: Semigroup Action where
  append (Action a1) (Action a2) = Action (a1 >=> a2)

instance monoidAction :: Monoid Action where
  mempty = Action pure

newtype ActionMonad a = ActionMonad (ReaderT ActionAnswer Effect a)

type ActionAnswer =
  { wsClient :: Y_Ws_Client
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
