module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import System.IO
import Control.Monad.IO.Class (liftIO)

import Hordle.Hordle
import Hordle.Game (aiGameWithWord)

prop_solver :: Property
prop_solver = monadicIO $ do
  t <- liftIO $ targets
  n <- run $ generate $ choose (0, 2000)
  liftIO $ aiGameWithWord stdout (t !! n)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Test the solver" prop_solver
        ]

