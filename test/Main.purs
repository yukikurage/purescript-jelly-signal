module Test.Main where

import Prelude

import Data.Functor.Contravariant (cvoid, (>#<), (>$), (>$<))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Class.Console (log)
import Palindrome.Contravariant (Handler, pulse, runner)
import Palindrome.Signal (newState, sample, sampleBy)

logger :: Handler String
logger = log >$< runner

loggerShow :: forall a. Show a => Handler a
loggerShow = show >$< logger

helloHandler :: forall a. Handler a
helloHandler = "Hello, World!" >$ logger

helloWithTimeHandler :: Handler Number
helloWithTimeHandler = logger >#< \n -> "Hello from " <> show n <> "ms"

main :: Effect Unit
main = do
  pulse 1000 helloHandler
  pulse 1000 $ helloWithTimeHandler

  stateS /\ stateH <- newState 0

  pulse 300 $ cvoid $ sample stateS loggerShow
  pulse 1000 $ cvoid $ sampleBy stateS stateH >#< \_ -> add 1
