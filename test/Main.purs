module Test.Main where

import Prelude

import Control.Apply (lift3)
import Data.Identity (Identity)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import Signal (Signal, newChannel, runSignal, send, subscribe)
import Test.Spec (SpecT, it, sequential)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

traceSignal :: forall a. Signal a -> Effect (Effect (Array a))
traceSignal sig = do
  ref <- new []
  stop <- runSignal $ sig <#> \a -> do
    modify_ (_ <> [ a ]) ref
    mempty
  pure do
    stop
    read ref

pureTest :: SpecT Aff Unit Identity Unit
pureTest = it "pure" do
  let
    test = pure 1
  trace <- liftEffect $ traceSignal test
  res <- liftEffect trace
  res `shouldEqual` [ 1 ]

mapTest :: SpecT Aff Unit Identity Unit
mapTest = it "mapN" do
  let
    test = lift3 (\a b c -> a + b + c) (pure 1) (pure 2) (pure 3)
  trace <- liftEffect $ traceSignal test
  res <- liftEffect trace
  res `shouldEqual` [ 6 ]

channelTest :: SpecT Aff Unit Identity Unit
channelTest = it "channel" do
  chn1 <- newChannel 0
  chn2 <- newChannel 0
  let
    test = lift3 (\a b c -> a + b + c) (subscribe chn1) (subscribe chn2) (pure 3)
  trace <- liftEffect $ traceSignal test
  send chn1 1
  send chn2 2
  res <- liftEffect trace
  res `shouldEqual` [ 3, 4, 6 ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ sequential do
  pureTest
  mapTest
