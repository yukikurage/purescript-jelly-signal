module Signal where

import Prelude

import Control.Apply (lift2)
import Data.Functor (mapFlipped)
import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data Signal :: Type -> Type
foreign import data Process :: Type

-- | ( (a -> Effect Unit) {- Callback -}
-- |   -> Effect a {- Initialization & return initial value -}
-- | )
foreign import newSignal :: forall a. ((a -> Effect Unit) -> Effect a) -> Effect (Signal a)
foreign import pureImpl :: forall a. a -> Signal a
foreign import mapImpl :: forall a b. (a -> b) -> Signal a -> Signal b
foreign import applyImpl :: forall a b. Signal (a -> b) -> Signal a -> Signal b
foreign import runSignal :: Signal (Effect Unit) -> Effect Process
foreign import mergeWith :: forall a. (a -> a -> a) -> Signal a -> Signal a -> Signal a
foreign import getImpl :: forall a. (a -> Maybe a) -> Maybe a -> Process -> Signal a -> Effect a

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

runSignal_ :: Signal (Effect Unit) -> Effect Unit
runSignal_ = void <<< runSignal

get :: forall a. Process -> Signal a -> Effect a
get = getImpl Just Nothing
