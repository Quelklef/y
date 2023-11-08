module Y.Client.Action where

import Prelude

import Data.Tuple.Nested (type (/\))

import Effect (Effect)
import Effect.Class (class MonadEffect)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Reader.Trans (ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk, ask)
import Control.Monad.Writer.Trans (WriterT, runWriterT, tell)

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

newtype AfterRenderEffect = AfterRenderEffect (Effect Unit)
newtype ActionMonad a = ActionMonad (ReaderT ActionAnswer (WriterT AfterRenderEffect Effect) a)

instance Semigroup AfterRenderEffect where
  append (AfterRenderEffect a1) (AfterRenderEffect a2) = AfterRenderEffect do
     _ <- a1
     _ <- a2
     (pure unit)

instance Monoid AfterRenderEffect where
  mempty = AfterRenderEffect (pure unit)

type ActionAnswer =
  { wsClient :: Y_Ws_Client
  }

runAction :: ActionAnswer -> Action -> (Model -> Effect (Model /\ AfterRenderEffect))
runAction answer (Action action) model =
  case (action model) of ActionMonad mr -> runWriterT (runReaderT mr answer)

unActionMonad :: forall a. ActionMonad a -> ReaderT ActionAnswer (WriterT AfterRenderEffect Effect) a
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
  liftEffect ef = ActionMonad (lift (lift ef))

instance monadReaderActionMonad :: MonadAsk ActionAnswer ActionMonad where
  ask = ActionMonad ask

afterRender :: Effect Unit -> ActionMonad Unit
afterRender effect =
  let (writer :: WriterT AfterRenderEffect Effect Unit) = tell (AfterRenderEffect effect)
  in ActionMonad $ lift $ writer
