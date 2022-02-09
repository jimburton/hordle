module Main where

import Test.QuickCheck ( Property )
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Framework ( defaultMain, Test )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import System.IO ( openFile, IOMode(WriteMode) )
import Control.Monad.IO.Class (liftIO)

import Hordle.Types (Game(..), HintFunction)
import qualified Hordle.Solver.LookAhead as LA
import qualified Hordle.Solver.Solve as HS
import Hordle.UI.Solver (solve)

prop_solverLA :: Property
prop_solverLA = prop_solver LA.hint 

prop_solverS :: Property
prop_solverS = prop_solver HS.hint

prop_solver :: HintFunction -> Property
prop_solver hf = monadicIO $ do
  h <- liftIO $ openFile "/dev/null" WriteMode
  g <- liftIO $ solve h hf
  assert (_success g)

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ testProperty "Test the LookAhead solver" prop_solverLA
        , testProperty "Test the Real solver" prop_solverS
        ]

