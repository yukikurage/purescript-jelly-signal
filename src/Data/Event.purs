module Data.Event where

import Prelude

import Control.Alt (class Alt)
import Control.Bind (bindFlipped)
import Control.Plus (class Plus)
import Data.Channel (Channel, newChannel, sendChannel, subscribeChannel)
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Filterable (class Compactable, class Filterable)
import Data.Foldable (class Foldable, traverse_)
import Data.Int (floor)
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Now (now)
import Effect.Ref (new, read, write)
import Effect.Timer (IntervalId, setInterval, setTimeout)

-- | Event is a sequence of sented value from the time they are subscribed to until they are unsubscribed.
-- | Values sented at time other than this are ignored.
-- | This makes functions such as scan pure.
newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))

instance Functor Event where
  map f (Event sbs) = Event \hdl -> sbs (hdl <<< f)

instance Alt Event where
  alt = append

instance Plus Event where
  empty = mempty

instance Semigroup (Event a) where
  append (Event sbs1) (Event sbs2) = Event \hdl -> do
    uns1 <- sbs1 hdl
    uns2 <- sbs2 hdl
    pure $ uns1 *> uns2

instance Monoid (Event a) where
  mempty = Event \_ -> pure $ pure unit

instance Compactable Event where
  compact (Event sbs) = Event \hdl -> sbs case _ of
    Just a -> hdl a
    Nothing -> pure unit
  separate (Event sbs) =
    let
      left = Event \hdl -> sbs case _ of
        Left a -> hdl a
        Right _ -> pure unit
      right = Event \hdl -> sbs case _ of
        Left _ -> pure unit
        Right b -> hdl b
    in
      { left, right }

instance Filterable Event where
  filterMap f (Event sbs) = Event
    \hdl -> sbs \a -> case f a of
      Nothing -> pure unit
      Just b -> hdl b
  filter f (Event sbs) = Event
    \hdl -> sbs \a -> if f a then hdl a else pure unit
  partitionMap f (Event sbs) =
    let
      left = Event \hdl -> sbs \a -> case f a of
        Left b -> hdl b
        Right _ -> pure unit
      right = Event \hdl -> sbs \a -> case f a of
        Left _ -> pure unit
        Right b -> hdl b
    in
      { left, right }
  partition f (Event sbs) =
    let
      no = Event \hdl -> sbs \a -> if f a then hdl a else pure unit
      yes = Event \hdl -> sbs \a -> if f a then pure unit else hdl a
    in
      { no, yes }

-- | Make event from channel.
fromChannel :: forall a. Channel a -> Event a
fromChannel chn = Event $ subscribeChannel chn

-- | Make new Event.
-- | The passed Effect is executed immediately and registers the hdl.
newEvent :: forall a. ((a -> Effect Unit) -> Effect Unit) -> Effect (Event a)
newEvent emitter = do
  chn <- newChannel
  emitter (sendChannel chn)
  pure $ fromChannel chn

-- | Subecribe to the event and return the uns effect.
subscribe :: forall a. Event a -> (a -> Effect Unit) -> Effect (Effect Unit)
subscribe (Event sbs) hdl = sbs hdl

-- | Unwrap un Effectibe Event
unwrap :: forall a. Event (Effect a) -> Event a
unwrap (Event sbs) = Event \hdl -> sbs (bindFlipped hdl)

-- | Merge multiple events.
-- | This function is more performant than using `append` multiple times.
mergeMany :: forall a. Array (Event a) -> Event a
mergeMany events = Event \hdl -> do
  unss <- traverse (\(Event sbs) -> sbs hdl) events
  pure $ traverse_ identity unss

-- | A function is applied that takes one previous value and the current value and returns a new value.
scan :: forall a b. (b -> a -> b) -> b -> Event a -> Event b
scan f initial (Event sbs) = Event \hdl -> do
  acc <- new initial
  uns <- sbs \a -> do
    b <- read acc
    let b' = f b a
    write b' acc
    hdl b'
  pure uns

-- | scanEvent, but the initial value is taken from the first value sented.
scan1 :: forall a. (a -> a -> a) -> Event a -> Event a
scan1 f (Event sbs) = Event \hdl -> do
  acc <- new Nothing
  uns <- sbs \a -> do
    mb <- read acc
    case mb of
      Nothing -> do
        write (Just a) acc
        hdl a
      Just b -> do
        let b' = f b a
        write (Just b') acc
        hdl b'
  pure uns

-- | scanEvent, but scan function is `append` of Semigroup.
scanSemigroup :: forall a. Semigroup a => a -> Event a -> Event a
scanSemigroup = scan append

-- | scanSemigroupEvent, but the initial value is taken from the first value sented.
scanSemigroup1 :: forall a. Semigroup a => Event a -> Event a
scanSemigroup1 = scan1 append

-- | Drop value that equals to the previous value.
dropEq :: forall a. Eq a => Event a -> Event a
dropEq (Event sbs) = Event \hdl -> do
  acc <- new Nothing
  uns <- sbs \a -> do
    mb <- read acc
    case mb of
      Nothing -> do
        write (Just a) acc
        hdl a
      Just b -> do
        unless (a == b) do
          write (Just a) acc
          hdl a
  pure uns

-- | Separately fire each of the arrays
flatten :: forall f a. Foldable f => Event (f a) -> Event a
flatten (Event sbs) = Event \hdl -> do
  uns <- sbs (traverse_ hdl)
  pure uns

-- | Emit the value 
every :: Int -> Effect (Event Int /\ IntervalId)
every n = do
  chn <- newChannel
  Milliseconds st <- unInstant <$> now
  id <- setInterval n do
    Milliseconds et <- unInstant <$> now
    let i = et - st
    sendChannel chn $ floor i
  pure $ fromChannel chn /\ id

-- | Emit the value
every_ :: Int -> Effect (Event Int)
every_ n = fst <$> every n

delay :: forall a. Int -> Event a -> Event a
delay n (Event sbs) = Event \hdl -> do
  frgR <- new true
  uns <- sbs \a -> do
    void $ setTimeout n do
      frg <- read frgR
      when frg $ hdl a
  pure (write false frgR *> uns)

-- | Emit the value once after subscribed
just :: forall a. a -> Event a
just a = Event \hdl -> hdl a $> mempty

-- | Emit the effective value once after subscribed
justE :: forall a. (Effect a) -> Event a
justE eff = Event \hdl -> eff >>= hdl $> mempty

-- | Emit the value once after subscribed
after :: forall a. Int -> a -> Event a
after n a = Event \hdl -> do
  frgR <- new true
  void $ setTimeout n do
    frg <- read frgR
    when frg $ hdl a
  pure $ write false frgR
