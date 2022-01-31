{-|
Module      : Hordle.UI
Description : CLI for playing Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

CLI for playing Hordle.
-}
{-# LANGUAGE OverloadedStrings #-}
module Hordle.UI
  (gameOver
  , drawGrid
  , helpText)
  where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List (intercalate)
import           Lens.Micro ((^.))

import           Hordle.Hordle
  (CharInfo(..)
  , Game
  , word
  , attempts
  , success)

-- | Print a message when the game is over.
gameOver :: Game -> IO ()
gameOver g = if g ^. success
             then TIO.putStrLn "Well done!"
             else TIO.putStrLn $ "Hard luck! The word was " <> (g ^. word)

-- | Draw the game grid.
drawGrid :: Game -> IO ()
drawGrid g = do
  let as = reverse $ g ^. attempts
  TIO.putStrLn hline
  drawLines as 0
  where hline  = "+-------------------+"
        iline  = "|   |   |   |   |   |"
        line a = T.pack $ "|" <> intercalate "|"
          (map (\(c,i) -> " "<>colour i<>[c]<>def<>" ") a) <> "|"
        colour (Green _)  = green
        colour Black      = red
        colour (Yellow _) = yellow
        drawLines _  6 = pure ()
        drawLines as n = do if n < length as
                              then TIO.putStrLn $ line (as!!n)
                              else TIO.putStrLn iline
                            TIO.putStrLn hline 
                            drawLines as (n+1)
        red    = "\ESC[31m"
        green  = "\ESC[32m"
        yellow = "\ESC[33m"
        def    = "\ESC[0m"

-- | Explain the colours to the user.
helpText :: Text
helpText = "\ESC[32mchar in right place.\ESC[0m\n"
           <> "\ESC[33mchar in word but wrong place.\ESC[0m\n"
           <> "\ESC[31mchar not in word.\ESC[0m"
