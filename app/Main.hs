{-|
Module      : Main (for the Hordle app)
Description : Entry point for Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Entry point for Hordle.
-}
module Main where

import qualified Data.Text.IO as TIO
import Hordle.Hordle
import Hordle.UI.UI

main :: IO ()
main = do
  TIO.putStrLn helpText
  g <- initGame
  -- TIO.putStrLn (g ^. word)
  playGame g
  -- allAIWords
