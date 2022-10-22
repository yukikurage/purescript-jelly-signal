module Signal.Time where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Signal (Signal, newSignal)

every :: Int -> Effect (Signal Int)
every dt = newSignal \callback -> do
  Milliseconds st <- unInstant <$> now
  _ <- setInterval dt do
    Milliseconds nt <- unInstant <$> now
    callback (floor $ nt - st)
  pure 0
