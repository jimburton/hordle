{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hordle.UI.Solver
Description : Frontend for the solver.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Frontend for the solver. The solver uses a greedy, backtracking minimax
algorithm. It maintains a "testbed" game with no knowledge of the secret word
but whose info map is kept in sync with that of the real game. Candidate words
are submitted as guesses to the testbed game, then one which yields the fewest
subsequent possibilities is chosen.
-}
module Hordle.UI.Solver
  (solve
  , solveWithWord
  , solveAll ) where

import           System.IO
import           Lens.Micro ((^.))
import           Data.Time (getCurrentTime) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Hordle.Hordle
  ( initGame
  , initGameWithWord
  , firstGuess
  , doGuess )
import           Hordle.Types
  ( Game
  , word
  , done
  , HintFunction )
import           Hordle.Dict (targets)
import qualified Hordle.Solver.Internal as HSI
import           Hordle.UI.UI (logEntry)

-- * Solving puzzles

-- | Start a game with a random target and a solver.
solve :: Handle -> HintFunction -> IO Game
solve h hf = do
  g <- initGame
  solveTurn h hf (firstGuess g)

-- | Start a game with a given word and a solver.
solveWithWord :: Handle -> HintFunction -> Text -> IO Game
solveWithWord h hf w = solveTurn h hf (firstGuess $ initGameWithWord w)

-- | Run the solver against all words.
solveAll :: HintFunction -> IO ()
solveAll hf = do
  h <- openFile "etc/solver-lookahead.log" WriteMode
  begin <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show begin)
  targets >>= mapM_ (solveWithWord h hf)
  end <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show end)
  hClose h

-- | Take a turn in an automated game with a given hint function.
solveTurn :: Handle
          -> HintFunction
          -> Game
          -> IO Game
solveTurn h hf g =
  if g ^. done
  then do TIO.hPutStrLn h (logEntry g)
          hFlush h
          pure g
  else do mg <- hf g
          case mg of
            Nothing  -> do -- TIO.putStrLn "Backtracking."
                           solveTurn h hf (HSI.backtrack g)
            (Just t) -> do -- TIO.putStrLn ("Trying "<>t)
                           let g' = doGuess g t
                           if t == g' ^. word
                             then do TIO.hPutStrLn h (logEntry g')
                                     hFlush h
                                     pure g'
                             else solveTurn h hf g'
