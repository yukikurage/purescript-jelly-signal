module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Signal (Signal, runSignal_)
import Signal.Time (every)

helloTick :: Signal Int -> Signal String
helloTick tick = map (\n -> "Hello " <> show n) tick

mulExpr :: Int -> Int -> String
mulExpr a b = show a <> " * " <> show b <> " = " <> show (a * b)

main :: Effect Unit
main = do
  pulse1 <- every 1000
  pulse2 <- every 2000
  runSignal_ $ mulExpr <$> pulse1 <*> pulse2 <#> log
