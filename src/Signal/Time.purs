module Signal.Time where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Signal (Signal, newChannel, send, subscribe)

every :: Int -> Effect (Signal Int)
every dt = do
  chn <- newChannel
  Milliseconds st <- unInstant <$> now
  _ <- setInterval dt do
    Milliseconds nt <- unInstant <$> now
    send chn $ floor $ nt - st
  pure $ subscribe chn do
    Milliseconds nt <- unInstant <$> now
    pure $ floor $ nt - st
