module Test.Main where

import Prelude

import Data.Identity (Identity)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchAff_)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import Signal (Signal, runSignal_)
import Test.Spec (SpecT, it, sequential)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

traceSignal :: forall a. Int -> Signal a -> Aff (Array a)
traceSignal n s = do
  results <- liftEffect $ new []
  liftEffect $ runSignal_ $ s <#> \a -> do
    modify_ (_ <> [ a ]) results
  delay $ Milliseconds $ toNumber n
  liftEffect $ read results

pureTest :: SpecT Aff Unit Identity Unit
pureTest = it "`pure` returns just one times" do
  let
    test = pure 1
  res <- liftAff $ traceSignal 1 test
  res `shouldEqual` [ 1 ]

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] $ sequential do
  pureTest
