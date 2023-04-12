{-|
Module      : Main (for the Hordle app)
Description : Entry point for Hordle.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Entry point for Hordle.
-}
module Main where

import qualified Data.Text.IO as TIO
import Hordle.Hordle ( initGame )
import Hordle.UI.UI ( playGame, helpText )

main :: IO ()
main = do
  TIO.putStrLn helpText
  g <- initGame
  -- TIO.putStrLn (g ^. word)
  playGame g
  -- allAIWords
