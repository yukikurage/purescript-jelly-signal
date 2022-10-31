module Signal.Time
  ( every
  ) where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Now (now)
import Effect.Timer (setInterval)
import Signal (Signal, newChannel, send, subscribe)

nowMilliseconds :: Effect Number
nowMilliseconds = unwrap <<< unInstant <$> now

every :: Int -> Effect (Signal Int)
every dt = do
  chn <- newChannel
  st <- nowMilliseconds
  _ <- setInterval dt do
    nt <- nowMilliseconds
    send chn $ floor $ nt - st
  pure $ subscribe chn $ floor <$> nowMilliseconds
