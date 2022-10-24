module Data.Channel where

import Prelude

import Effect (Effect)

-- | Channels are events whose values can be sent manually.
-- | It can be changed to an Event with Data.Event.fromChannel.
foreign import data Channel :: Type -> Type

-- | Make a new channel
foreign import newChannel :: forall a. Effect (Channel a)

-- | Send value to channel
foreign import sendChannel :: forall a. Channel a -> a -> Effect Unit

-- | Subscribe channel and return unsubscribe effect
foreign import subscribeChannel :: forall a. Channel a -> (a -> Effect Unit) -> Effect (Effect Unit)