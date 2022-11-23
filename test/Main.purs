module Test.Main where

import Prelude

import Control.Apply (lift3)
import Data.Identity (Identity)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import Jelly.Signal (Signal, memoSignal, newChannel, runSignal, subscribe, writeChannel)
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

applyTest :: SpecT Aff Unit Identity Unit
applyTest = it "mapN" do
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
  writeChannel chn1 1
  writeChannel chn2 2
  res <- liftEffect trace
  res `shouldEqual` [ 3, 4, 6 ]

joinTest :: SpecT Aff Unit Identity Unit
joinTest = it "join" do
  chn1 <- newChannel 0
  chn2 <- newChannel 0
  chn3 <- newChannel 0
  let
    test = do
      a <- subscribe chn1
      b <- subscribe chn2
      _ <- subscribe chn3
      pure $ a + b
  trace <- liftEffect $ traceSignal test
  writeChannel chn1 1
  writeChannel chn2 2
  writeChannel chn3 3
  res <- liftEffect trace
  res `shouldEqual` [ 0, 1, 3, 3 ]

memoTest :: SpecT Aff Unit Identity Unit
memoTest = it "memo" do
  chn <- newChannel 0
  let
    effective = subscribe chn <#> \a -> pure $ Tuple a mempty
  Tuple test stop <- memoSignal effective
  trace <- liftEffect $ traceSignal test
  writeChannel chn 1
  liftEffect $ stop
  writeChannel chn 2
  res <- liftEffect trace
  res `shouldEqual` [ 0, 1 ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ sequential do
  pureTest
  applyTest
  joinTest
  memoTest
