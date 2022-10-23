module Signal where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)

foreign import data Signal :: Type -> Type

-- | ( (a -> Effect Unit) {- Callback -}
-- |   -> Effect a {- Initialization & return initial value -}
-- | )
foreign import newSignal :: forall a. ((a -> Effect Unit) -> Effect a) -> Effect (Signal a)
foreign import mapMany :: forall a b. (Array a -> b) -> Array (Signal a) -> Signal b
foreign import map0 :: forall a. a -> Signal a
foreign import map1 :: forall a b. (a -> b) -> Signal a -> Signal b
foreign import map2 :: forall a b c. (a -> b -> c) -> Signal a -> Signal b -> Signal c
foreign import map3 :: forall a b c d. (a -> b -> c -> d) -> Signal a -> Signal b -> Signal c -> Signal d
foreign import map4 :: forall a b c d e. (a -> b -> c -> d -> e) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e
foreign import map5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> Signal a -> Signal b -> Signal c -> Signal d -> Signal e -> Signal f

foreign import mergeManyWith :: forall a. (Array a -> a) -> Array (Signal a) -> Signal a
foreign import mergeManyImpl :: forall a. (forall b. b -> Maybe b) -> (forall b. Maybe b) -> Array (Signal a) -> Maybe (Signal a)
foreign import mergeWith :: forall a. (a -> a -> a) -> Signal a -> Signal a -> Signal a
foreign import runSignal :: Signal (Effect Unit) -> Effect Unit
foreign import foldp :: forall a b. (a -> b -> b) -> b -> Signal a -> Signal b

instance Functor Signal where
  map = map1

instance Apply Signal where
  apply = map2 \f a -> f a

instance Applicative Signal where
  pure = map0

instance Semigroup a => Semigroup (Signal a) where
  append = map2 append

instance Monoid a => Monoid (Signal a) where
  mempty = map0 mempty

mergeMany :: forall a. Array (Signal a) -> Maybe (Signal a)
mergeMany = mergeManyImpl Just Nothing

merge :: forall a. Signal a -> Signal a -> Signal a
merge = mergeWith const
