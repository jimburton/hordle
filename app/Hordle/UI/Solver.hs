{-# LANGUAGE OverloadedStrings #-}
module Hordle.UI.Solver where

import           System.IO
import           Lens.Micro ((&),(^.),(.~))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Hordle.UI.UI
import           Hordle.Hordle
import           Hordle.Types
import           Hordle.Solver.Solve (hint, processInfo)
import qualified Hordle.Solver.Internal as HSI

-- | 
solve :: Handle -> IO ()
solve h = do
  g <- initGame
  TIO.putStrLn ("SOLVING: "<> g ^. word)
  let rg = firstGuess g
      pg = rg & word .~ ""
  solveTurn h (rg, pg)

-- | 
solveTurn :: Handle -> (Game, Game) -> IO ()
solveTurn h (rg,pg) =
  if rg ^. done
  then gameOver rg
  else do mg <- hint pg
          case mg of
            Nothing  -> do TIO.hPutStrLn h "Backtracking."
                           solveTurn h (HSI.backtrack rg, HSI.backtrack pg)
            (Just t) -> do TIO.hPutStrLn h ("Trying "<>t)
                           if t == rg ^. word
                             then TIO.putStrLn $ "SUCCESS in "<>T.pack (show $ pg ^. numAttempts)
                             else do let pg' = processInfo t (rg ^. word) pg 
                                     solveTurn h (rg,pg')
