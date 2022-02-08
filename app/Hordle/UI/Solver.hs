{-# LANGUAGE OverloadedStrings #-}
module Hordle.UI.Solver where

import           System.IO
import           Lens.Micro ((&),(^.),(.~))
import           Data.Time (getCurrentTime) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Hordle.Hordle (initGameWithWord, firstGuess)
import           Hordle.Types
  ( Game
  , word
  , numAttempts
  , done
  , success )
import           Hordle.Dict (targets, getTarget)
import           Hordle.Solver.Solve (hint, processInfo)
import qualified Hordle.Solver.Internal as HSI

-- | 
solve :: Handle -> IO Game
solve h = getTarget >>= solveWithWord h

-- | 
solveTurn :: Handle -> (Game, Game) -> IO Game
solveTurn h (rg,pg) =
  if pg ^. done
  then do let pg' = setDone pg (rg ^. word)
          TIO.hPutStrLn h (logEntry pg')
          hFlush h
          pure pg'
  else do mg <- hint pg
          case mg of
            Nothing  -> do TIO.putStrLn "Backtracking."
                           solveTurn h (HSI.backtrack rg, HSI.backtrack pg)
            (Just t) -> do TIO.putStrLn ("Trying "<>t)
                           if t == rg ^. word
                             then do let pg' = setDone pg t
                                     TIO.hPutStrLn h (logEntry pg')
                                     hFlush h
                                     pure pg'
                             else do let pg' = processInfo t (rg ^. word) pg 
                                     solveTurn h (rg,pg')

setDone :: Game -> Text -> Game
setDone g t = g & word .~ t
                & success .~ (g ^. numAttempts < 7)
                & done .~ True
  
logEntry :: Game -> Text
logEntry g = "WORD: "<> g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
  
-- | Start a game with a given word and a solver.
solveWithWord :: Handle -> Text -> IO Game
solveWithWord h w = do
  TIO.putStrLn ("SOLVING: "<> w)
  let rg = firstGuess $ initGameWithWord w
      pg = rg & word .~ ""
  solveTurn h (rg, pg)

-- | Run the solver against all words.
solveAll :: IO ()
solveAll = do
  h <- openFile "etc/solver.log" WriteMode
  begin <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show begin)
  hFlush h
  targets >>= mapM_ (solveWithWord h)
  end <- getCurrentTime
  TIO.hPutStrLn h (T.pack $ show end)
  hClose h

