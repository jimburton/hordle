{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hordle.Game
Description : Functions for playing a game of Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for playing a game of Hordle.
-}
module Hordle.UI.LookAhead where

import           Lens.Micro ((&),(^.),(.~))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime) 
import           System.IO
import           System.Console.Haskeline
import           Data.Functor (($>))
import           Hordle.Hordle
import           Hordle.Types
import           Hordle.Dict
import qualified Hordle.Solver.Internal as HSI
import qualified Hordle.Solver.LookAhead as LA

-- * Playing the game.


-- | Start a game with a random target and a solver.
solve :: Handle -> IO Game
solve h = do
  g <- initGame
  solveTurn (LA.firstGuess g) h

-- | Start a game with a given word and a solver.
solveWithWord :: Handle -> Text -> IO Game
solveWithWord h w = solveTurn (LA.firstGuess $ initGameWithWord w) h

-- | Allow the LookAhead solver to take guesses until the game is over.
solveTurn :: Game -> Handle -> IO Game
solveTurn g h = do
  -- drawGrid g
  if g ^. done
    then do let t = "WORD: "<>g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
            TIO.hPutStrLn h t
            hFlush h
            pure g
    else do
    ht <- LA.hint g
    case ht of
      Nothing  -> solveTurn (HSI.backtrack g) h
      (Just t) -> solveTurn (LA.doGuess g t) h

-- | Run the solver against all words.
solveAll :: IO ()
solveAll = do
  h <- openFile "etc/solver-lookahead.log" WriteMode
  begin <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show begin)
  targets >>= mapM_ (solveWithWord h)
  end <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show end)
  hClose h
