module Jelly.Channel where

import Prelude

import Effect (Effect)

foreign import data Channel :: Type -> Type

foreign import newChannel :: forall a. Effect (Channel a)
foreign import send :: forall a. Channel a -> a -> Effect Unit
foreign import subscribe :: forall a. Channel a -> (a -> Effect Unit) -> Effect (Effect Unit)