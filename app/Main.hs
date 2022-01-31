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

import           Hordle.Hordle (initGame)
import           Hordle.UI (helpText)
import           Hordle.Game (playGame, allAIWords)

main :: IO ()
main = do --TIO.putStrLn helpText
          --g <- initGame
          -- TIO.putStrLn (g ^. word)
          --playGame g
  allAIWords
