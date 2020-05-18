module Main where

import           Test.QuickCheck
import qualified Test.Radix

-- | Test fixture entry point.
main :: IO ()
main = Test.Radix.run
