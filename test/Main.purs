module Test.Main where

import Prelude

import Data.FoldableWithIndex (allWithIndex)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)