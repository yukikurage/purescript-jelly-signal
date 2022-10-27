module Palindrome.Signal where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref (new, read, write)
import Palindrome.Contravariant (Handler, makeHandler, send)

newtype Signal a = Signal (Effect a)

derive newtype instance Functor Signal

makeSignal :: forall a. Effect a -> Signal a
makeSignal = Signal

readSignal :: forall a. Signal a -> Effect a
readSignal (Signal e) = e

newState :: forall a. a -> Effect (Signal a /\ Handler a)
newState a = do
  ref <- new a
  pure $ Signal (read ref) /\ makeHandler (\v -> write v ref)

sampleBy :: forall a b. Signal a -> Handler b -> Handler (a -> b)
sampleBy (Signal e) h = makeHandler $ \f -> send h =<< f <$> e

sample :: forall a. Signal a -> Handler a -> Handler Unit
sample (Signal e) h = makeHandler $ \_ -> send h =<< e

newTimer :: Effect (Signal Number)
newTimer = do
  Milliseconds st <- unInstant <$> now
  pure $ Signal $ do
    Milliseconds et <- unInstant <$> now
    pure $ et - st
