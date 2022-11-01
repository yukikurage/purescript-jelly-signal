module Signal
  ( Channel
  , Signal
  , channel
  , mutate
  , readSignal
  , runSignal
  , send
  , signal
  , subscribe
  , watchSignal
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write)

-- | Channel is a type that represents value input.
foreign import data Channel :: Type -> Type

foreign import channelImpl :: forall a. a -> Effect (Channel a)

-- | Make new Channel.
channel :: forall m a. MonadEffect m => a -> m (Channel a)
channel = liftEffect <<< channelImpl

foreign import mutateImpl :: forall a. Channel a -> (a -> a) -> Effect Unit

-- | Mutate Channel value.
mutate :: forall m a. MonadEffect m => Channel a -> (a -> a) -> m Unit
mutate c f = liftEffect $ mutateImpl c f

-- | Send value to Channel.
send :: forall m a. MonadEffect m => Channel a -> a -> m Unit
send c a = mutate c (const a)

foreign import readChannel :: forall a. Channel a -> Effect a

foreign import subscribeChannel :: forall a. Channel a -> (a -> Effect (Effect Unit)) -> Effect (Effect Unit)

-- | Signal is a type that represents value output.
newtype Signal a = Signal { run :: (a -> Effect (Effect Unit)) -> Effect (Effect Unit), get :: Effect a }

-- | Subscribe to Channel and make Signal.
subscribe :: forall a. Channel a -> Signal a
subscribe chn = Signal { run: subscribeChannel chn, get: readChannel chn }

-- | Run Effective Signal.
runSignal :: forall m. MonadEffect m => Signal (Effect (Effect Unit)) -> m (Effect Unit)
runSignal (Signal { run }) = liftEffect $ run identity

-- | Run Signal without initialization.
watchSignal :: forall m. MonadEffect m => Signal (Effect (Effect Unit)) -> m (Effect Unit)
watchSignal sig = do
  isInit <- liftEffect $ new true
  runSignal $ sig <#> \eff -> do
    init <- read isInit
    if init then write false isInit *> mempty else eff

-- | Read Signal value.
readSignal :: forall m a. MonadEffect m => Signal a -> m a
readSignal (Signal { get }) = liftEffect get

-- | Make pair of Signal and Channel.
signal :: forall m a. MonadEffect m => a -> m (Tuple (Signal a) (Channel a))
signal a = do
  chn <- channel a
  pure $ Tuple (subscribe chn) chn

instance Functor Signal where
  map f (Signal { run, get }) = Signal
    { run: \cb -> run (cb <<< f)
    , get: f <$> get
    }

instance Apply Signal where
  apply (Signal { run: runF, get: getF }) (Signal { run: runA, get: getA }) =
    Signal
      { run: \cb -> runF (\f -> runA (cb <<< f))
      , get: getF <*> getA
      }

instance Applicative Signal where
  pure a = Signal
    { run: \cb -> cb a
    , get: pure a
    }

instance Bind Signal where
  bind (Signal { run: runA, get: getA }) f =
    Signal
      { run: \cb -> runA (\a -> let Signal { run } = f a in run cb)
      , get: getA >>= \a -> let Signal { get } = f a in get
      }

instance Monad Signal

instance Semigroup a => Semigroup (Signal a) where
  append = lift2 append

instance Monoid a => Monoid (Signal a) where
  mempty = pure mempty
