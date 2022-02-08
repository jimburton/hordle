{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hordle.UI.LookAhead
Description : Frontend for the LookAhead solver.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Frontend for the LookAhead solver. This solver cheats, as it evaluates possible guesses
by making them and seeing how "good" they were, i.e. how few subsequent possible words
did they yield. As a result it may submit many more than 6 guesses.
-}
module Hordle.UI.LookAhead where

import           Lens.Micro ((^.))
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime) 
import           System.IO
import           Hordle.Hordle
  ( initGame
  , initGameWithWord
  , firstGuess
  , doGuess )
import           Hordle.Types
  ( Game
  , numAttempts
  , success
  , word
  , done )
import           Hordle.Dict (targets)
import qualified Hordle.Solver.Internal as HSI
import qualified Hordle.Solver.LookAhead as LA

-- * Playing the game.


-- | Start a game with a random target and a solver.
solve :: Handle -> IO Game
solve h = do
  g <- initGame
  solveTurn (firstGuess g) h

-- | Start a game with a given word and a solver.
solveWithWord :: Handle -> Text -> IO Game
solveWithWord h w = solveTurn (firstGuess $ initGameWithWord w) h

-- | Allow the LookAhead solver to take guesses until the game is over.
solveTurn :: Game -> Handle -> IO Game
solveTurn g h = do
  if g ^. done
    then do let t = "WORD: "<>g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
            TIO.hPutStrLn h t
            hFlush h
            pure g
    else do
    ht <- LA.hint g
    case ht of
      Nothing  -> solveTurn (HSI.backtrack g) h
      (Just t) -> solveTurn (doGuess g t) h

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
