module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import System.IO

import Hordle.Hordle

prop_solver :: Property
prop_solver = monadicIO $ do
  t <- targets
  n <- run $ generate $ choose (0, length targets)
  initAIGameWithWord stdout (t !! n)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Test the solver" prop_solver
        ]

