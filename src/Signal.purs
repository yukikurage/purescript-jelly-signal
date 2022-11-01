module Data.Signal
  ( Channel
  , Signal
  , mutate
  , newChannel
  , readSignal
  , runSignal
  , send
  , subscribe
  , watchSignal
  ) where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write)

foreign import data Signal :: Type -> Type
foreign import data Channel :: Type -> Type

foreign import newChannelImpl :: forall a. a -> Effect (Channel a)
foreign import mutateImpl :: forall a. Channel a -> (a -> a) -> Effect Unit
foreign import sendImpl :: forall a. Channel a -> a -> Effect Unit
-- | Convert Channel to Signal
foreign import subscribe :: forall a. Channel a -> Signal a
foreign import runSignalImpl :: Signal (Effect (Effect Unit)) -> Effect (Effect Unit)
foreign import pureImpl :: forall a. a -> Signal a
foreign import mapImpl :: forall a b. (a -> b) -> Signal a -> Signal b
foreign import applyImpl :: forall a b. Signal (a -> b) -> Signal a -> Signal b
foreign import readSignalImpl :: forall a. Signal a -> Effect a

instance Functor Signal where
  map = mapImpl

instance Apply Signal where
  apply = applyImpl

instance Applicative Signal where
  pure = pureImpl

instance Semigroup a => Semigroup (Signal a) where
  append = lift2 append

instance Monoid a => Monoid (Signal a) where
  mempty = pure mempty

-- | Create a new channel
newChannel :: forall m a. MonadEffect m => a -> m (Channel a)
newChannel a = liftEffect $ newChannelImpl a

-- | Mutate a channel with a function
mutate :: forall m a. MonadEffect m => Channel a -> (a -> a) -> m Unit
mutate c f = liftEffect $ mutateImpl c f

-- | Send a value to a channel
send :: forall m a. MonadEffect m => Channel a -> a -> m Unit
send c a = liftEffect $ sendImpl c a

-- | Run a signal, returning an effect which can be used to unsubscribe.
runSignal :: forall m. MonadEffect m => Signal (Effect (Effect Unit)) -> m (Effect Unit)
runSignal sig = liftEffect $ runSignalImpl sig

-- | Read Signal Immediately
readSignal :: forall m a. MonadEffect m => Signal a -> m a
readSignal sig = liftEffect $ readSignalImpl sig

-- | Run Signal without initialize
watchSignal :: forall m. MonadEffect m => Signal (Effect (Effect Unit)) -> m (Effect Unit)
watchSignal signal = do
  isInit <- liftEffect $ new true
  runSignal $ signal <#> \effect -> do
    isInit' <- read isInit
    if isInit' then do
      write false isInit
      mempty
    else
      effect
