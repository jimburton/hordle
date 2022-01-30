module Main where

import qualified Data.Text.IO as TIO

import           Hordle.Hordle (initGame)
import           Hordle.UI (helpText)
import           Hordle.Game (playGame)

main :: IO ()
main = do TIO.putStrLn helpText
          g <- initGame
          -- TIO.putStrLn (g ^. word)
          playGame g
