module Jelly.Signal
  ( Channel
  , Signal
  , ifSignal
  , memoSignal
  , modifyChannel
  , modifyChannel_
  , newChannel
  , newState
  , readChannel
  , readSignal
  , runSignal
  , subscribe
  , watchSignal
  , writeChannel
  ) where

import Prelude

import Control.Apply (lift2)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (new, read, write)
import Unsafe.Coerce (unsafeCoerce)

-- | Channel is a type that represents value input.
foreign import data Channel :: Type -> Type

foreign import newChannelImpl :: forall a. a -> Effect (Channel a)

-- | Make new Channel.
newChannel :: forall m a. MonadEffect m => a -> m (Channel a)
newChannel = liftEffect <<< newChannelImpl

foreign import modifyChannelImpl :: forall a. Channel a -> (a -> a) -> Effect Unit

foreign import readChannel :: forall a. Channel a -> Effect a

-- | Modify value in Channel, and return new value.
modifyChannel :: forall m a. MonadEffect m => Channel a -> (a -> a) -> m a
modifyChannel c f = liftEffect do
  modifyChannelImpl c f
  readChannel c

-- | Void version of `modifyChannel`.
modifyChannel_ :: forall m a. MonadEffect m => Channel a -> (a -> a) -> m Unit
modifyChannel_ c f = liftEffect $ modifyChannelImpl c f

-- | Write value to Channel.
writeChannel :: forall m a. MonadEffect m => Channel a -> a -> m Unit
writeChannel c a = modifyChannel_ c (const a)

foreign import subscribeChannel :: forall a. Channel a -> (a -> Effect (Effect Unit)) -> Effect (Effect Unit)

-- | Signal is a type that represents value output.
newtype Signal a = Signal { run :: (a -> Effect (Effect Unit)) -> Effect (Effect Unit), get :: Effect a }

-- | Subscribe to Channel and make Signal.
subscribe :: forall a. Channel a -> Signal a
subscribe chn = Signal { run: subscribeChannel chn, get: readChannel chn }

-- | Run Effective Signal.
runSignal :: forall m. MonadEffect m => Signal (Effect (Effect Unit)) -> m (Effect Unit)
runSignal (Signal { run }) = liftEffect $ run identity

-- | Memorize effective Signal value to another Signal.
memoSignal :: forall m a. MonadEffect m => Signal (Effect (Tuple a (Effect Unit))) -> m (Tuple (Signal a) (Effect Unit))
memoSignal sig = do
  chn <- newChannel $ unsafeCoerce unit -- Safe because write a value to channel immediately.
  cln <- runSignal $ sig <#> \eff -> do
    Tuple a cleaner <- eff
    writeChannel chn a
    pure cleaner
  pure $ Tuple (subscribe chn) cln

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
newState :: forall m a. MonadEffect m => a -> m (Tuple (Signal a) (Channel a))
newState a = do
  chn <- newChannel a
  pure $ Tuple (subscribe chn) chn

-- | Use conditional Signal.
ifSignal :: forall a. Signal Boolean -> a -> a -> Signal a
ifSignal sig a b = (if _ then a else b) <$> sig

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
