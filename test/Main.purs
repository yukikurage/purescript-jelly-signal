module Test.Main where

import Prelude

import Control.Apply (lift3)
import Data.FoldableWithIndex (allWithIndex)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Ref (modify_, new, read)
import Signal (Signal, foldp, runSignal)
import Signal.Time (every)
import Test.Spec (SpecT, it, sequential)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

traceSignal :: forall a. Int -> Signal a -> Aff (Array a)
traceSignal n s = do
  results <- liftEffect $ new []
  liftEffect $ runSignal $ s <#> \a -> do
    modify_ (_ <> [ a ]) results
  delay $ Milliseconds $ toNumber n
  liftEffect $ read results

pureTest :: SpecT Aff Unit Identity Unit
pureTest = it "pure" do
  let
    test = pure 1
  res <- liftAff $ traceSignal 1 test
  res `shouldEqual` [ 1 ]

mapTest :: SpecT Aff Unit Identity Unit
mapTest = it "mapN" do
  let
    test = lift3 (\a b c -> a + b + c) (pure 1) (pure 2) (pure 3)
  res <- liftAff $ traceSignal 1 test
  res `shouldEqual` [ 6 ]

everyTest :: SpecT Aff Unit Identity Unit
everyTest = it "every" do
  tick <- liftEffect $ every 10
  let
    count = foldp (\_ n -> n + 1) 0 tick
  res <- liftAff $ traceSignal 100 count
  logShow res
  res `shouldSatisfy` (allWithIndex eq)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ sequential do
  pureTest
  mapTest
  everyTest
