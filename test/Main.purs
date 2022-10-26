module Test.Main where

import Prelude

import Data.Functor.Contravariant ((>#<), (>$<))
import Effect (Effect)
import Effect.Class.Console (log)
import Palindrome.Contravariant (Handler, effecter, runner, send)

logger :: Handler String
logger = log >$< runner

effLogger :: Handler (Effect String)
effLogger = effecter logger

main :: Effect Unit
main = do
  send logger "Hello, World!"
  send effLogger do
    log "Before the effect"
    pure "Hello, World! 2"
