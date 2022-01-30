{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.List (intercalate)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Lens.Micro ((^.))
import           Hordle (CharInfo(..)
                        , Game
                        , done
                        , attempts
                        , success
                        , word
                        , initGame
                        , doGuess
                        , hints
                        , hint
                        , isDictWord
                        , backtrack
                        , guess )
                 
import           UI ()
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do TIO.putStrLn helpText
          g <- initGame
          TIO.putStrLn (g ^. word)
          playGame g

-- | Play the game by querying the user for words until they guess the word or have
-- | used their five guesses.
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
                       showHints g
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

aiGame :: IO ()
aiGame = do
  g <- initGame
  TIO.putStrLn (g ^. word)
  loop g 1
  where loop :: Game -> Int -> IO ()
        loop g0 i = do
          if g0 ^. done
            then gameOver g0
            else do h <- hint g0
                    case h of
                      Nothing  -> do let gs = case g0 ^. guess of
                                           Nothing  -> ""
                                           (Just t) -> t
                                     TIO.putStrLn $ "No hints for ["<>gs<> "]. Backtracking."
                                     loop (backtrack g0) i
                      (Just t) -> do TIO.putStrLn $ "Guess " <>T.pack (show i)<>": "<>t
                                     loop (doGuess g0 t) (i+1)

-- | Suggest some words based on the state of the game.
showHints :: Game -> IO ()
showHints g = hints g >>= mapM_ TIO.putStrLn

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
