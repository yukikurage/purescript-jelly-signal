module Signal
  ( Channel
  , Signal
  , foldp
  , merge
  , mergeWith
  , mutate
  , newChannel
  , runSignal
  , send
  , subscribe
  ) where

import Prelude

import Control.Apply (lift2)
import Effect (Effect)

foreign import data Signal :: Type -> Type
foreign import data Channel :: Type -> Type

-- | ( (a -> Effect Unit) {- Callback -}
-- |   -> Effect a {- Initialization & return initial value -}
-- | )
foreign import runSignal :: Signal (Effect Unit) -> Effect Unit
foreign import newChannel :: forall a. Effect (Channel a)
foreign import send :: forall a. Channel a -> a -> Effect Unit
foreign import mutate :: forall a. Channel a -> (a -> a) -> Effect Unit
foreign import subscribe :: forall a. Channel a -> Effect a -> Signal a
foreign import pureImpl :: forall a. a -> Signal a
foreign import mapImpl :: forall a b. (a -> b) -> Signal a -> Signal b
foreign import applyImpl :: forall a b. Signal (a -> b) -> Signal a -> Signal b
foreign import joinImpl :: forall a. Signal (Signal a) -> Signal a
foreign import mergeWith :: forall a. (a -> a -> a) -> Signal a -> Signal a -> Signal a
foreign import foldp :: forall a b. (a -> b -> b) -> b -> Signal a -> Signal b

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

merge :: forall a. Signal a -> Signal a -> Signal a
merge = mergeWith const
