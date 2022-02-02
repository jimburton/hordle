module Main where

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Framework
import Test.Framework.Providers.QuickCheck2
import System.IO
import Control.Monad.IO.Class (liftIO)

import Hordle.Hordle (Game(..))
import Hordle.Game (solve)

prop_solver :: Property
prop_solver = monadicIO $ do
  g <- liftIO solve
  assert (_success g)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Test the solver" prop_solver
        ]

