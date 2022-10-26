module Palindrome.Contravariant where

import Prelude

import Data.Decidable (class Decidable)
import Data.Decide (class Decide)
import Data.Divide (class Divide, (>*<))
import Data.Divisible (class Divisible, conquer)
import Data.Either (Either(..))
import Data.Functor.Contravariant (class Contravariant, (>$<))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Timer (setTimeout)

newtype Handler a = Handler (a -> Effect Unit)

instance Contravariant Handler where
  cmap f (Handler h) = Handler (h <<< f)

instance Divide Handler where
  divide f (Handler h1) (Handler h2) = Handler \a -> do
    h1 $ fst $ f a
    h2 $ snd $ f a

instance Decide Handler where
  choose f (Handler h1) (Handler h2) = Handler \a -> do
    case f a of
      Left a' -> h1 a'
      Right a' -> h2 a'

instance Divisible Handler where
  conquer = Handler \_ -> pure unit

instance Decidable Handler where
  lose f = Handler \a -> absurd $ f a

instance Semigroup (Handler a) where
  append h1 h2 = (\a -> Tuple a a) >$< h1 >*< h2

instance Monoid (Handler a) where
  mempty = conquer

runner :: Handler (Effect Unit)
runner = Handler identity

effecter :: forall a. Handler a -> Handler (Effect a)
effecter (Handler h) = Handler \e -> e >>= h

makeHandler :: forall a. (a -> Effect Unit) -> Handler a
makeHandler = Handler

send :: forall a. Handler a -> a -> Effect Unit
send (Handler h) = h

once :: Handler Unit -> Effect Unit
once (Handler h) = h unit

delay :: forall a. Int -> Handler a -> Handler a
delay n (Handler h) = Handler \a -> void $ setTimeout n (h a)

filter :: forall a. (a -> Boolean) -> Handler a -> Handler a
filter f (Handler h) = Handler \a -> if f a then h a else pure unit
