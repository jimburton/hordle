{-|
Module      : Hordle.UI
Description : Frontend for playing Hordle.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

CLI for playing Hordle.
-}
{-# LANGUAGE OverloadedStrings #-}
module Hordle.UI.UI
  ( playGame
  , logEntry
  , helpText
  , feedbackGame ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List (intercalate)
import           Lens.Micro ((^.))
import System.Console.Haskeline
    ( defaultSettings, getInputLine, runInputT, InputT )
import           Control.Monad.IO.Class (liftIO)
import           Hordle.Hordle
  ( doGuess
  , emptyGame
  , processInfo
  
  
   )
import           Hordle.Types
  ( Game
  , word
  , attempts
  , numAttempts
  , done
  , success
  , CharInfo(..))
import           Hordle.Dict (isDictWord)
import qualified Hordle.Solver.Solve as HS
import qualified Hordle.Solver.Internal as HSI

-- * CLI functions.

-- | Play the game by querying the user for words until they guess the word or have
-- | used their six guesses.
playGame :: Game -> IO ()
playGame g = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     if g ^. done
       then liftIO $ gameOver g
       else do mLn <- getInputLine ("Enter a five letter word [Attempt "
                                    <> show (1 + length (g ^. attempts))
                                    <> "]\n")
               case mLn of
                 Nothing -> loop
                 Just wdStr -> do
                   let attempt = T.toUpper $ T.pack wdStr
                   if attempt == ":HINT"
                     then liftIO $ do
                       showHint g
                       playGame g
                     else if T.length attempt /= 5
                          then liftIO $ do
                            TIO.putStrLn "Not a five letter word"
                            playGame g
                          else do
                     dw <- liftIO $ isDictWord attempt
                     if not dw
                       then liftIO $ do
                         TIO.putStrLn "Not a word from the dictionary"
                         playGame g
                       else liftIO $ do
                         let g' = doGuess g attempt
                         drawGrid g'
                         playGame g'

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

-- * Hints.

-- | Suggest a word based on the state of the game.
showHint :: Game -> IO ()
showHint g = HS.hint g >>= mapM_ TIO.putStrLn

-- * Feedback game.

-- | Play a game while entering the scores manually.
feedbackGame :: IO ()
feedbackGame = feedbackTurn emptyGame

-- | Take moves and their feedback scores until the game is done.
feedbackTurn :: Game -> IO ()
feedbackTurn g = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     if g ^. done
       then liftIO $ gameOver g
       else do mLn <- getInputLine "Enter a five letter word and its {G,Y,B} score\n"
               case mLn of
                 Nothing -> loop
                 Just wdStr -> do
                   let ws = T.words $ T.toUpper $ T.pack wdStr
                   if length ws == 2
                     then liftIO $ do
                     let attempt = head ws
                         sc      = ws !! 1
                         g'      = processInfo attempt sc g
                     if sc == "GGGGG"
                       then TIO.putStrLn $ "Done in "<>T.pack (show $ g' ^. numAttempts)<>" attempts."
                       else do
                       h <- HS.hint g'
                       case h of
                         Nothing  -> do TIO.putStrLn "Backtracking."
                                        feedbackTurn (HSI.backtrack g')
                         (Just t) -> do TIO.putStrLn ("Try "<>t)
                                        feedbackTurn g'
                     else liftIO $ do
                     TIO.putStrLn "Try again."
                     feedbackTurn g

-- | Format a log entry.
logEntry :: Game -> Text
logEntry g = "WORD: "<> g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
