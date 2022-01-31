{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Hordle.Game
Description : Functions for playing a game of Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Functions for playing a game of Hordle.
-}
module Hordle.Game (playGame
                   , aiGame
                   , aiGameWithWord
                   , feedbackGame
                   , allAIWords
                   , playGameAI
                   , showHints
                   , showHint
                   , problemAIWords) where

import           Lens.Micro ((^.))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.Text.IO as TIO
import           Data.Text (Text)
import qualified Data.Text as T
import           System.IO
import           System.Console.Haskeline

import           Hordle.Hordle
  (Game
  , done
  , attempts
  , numAttempts
  , word
  , success
  , initGame
  , initGameWithWord
  , emptyGame
  , doGuess
  , hints
  , hint
  , isDictWord
  , backtrack
  , targets
  , processInfo )
import           Hordle.UI (drawGrid, gameOver)

-- * Playing the game.

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

-- | Start a game with a random target and a solver.
aiGame :: IO ()
aiGame = do
  g <- initGame
  -- TIO.putStrLn (g ^. word)
  -- ts <- targets
  playGameAI g 1 stdout

-- | Start a game with a given word and a solver.
aiGameWithWord :: Handle -> Text -> IO ()
aiGameWithWord h w = playGameAI (initGameWithWord w) 1 h

-- | Run the solver against all words.
allAIWords :: IO ()
allAIWords = do
  h <- openFile "etc/solver.log" WriteMode 
  targets >>= mapM_ (aiGameWithWord h)
  hClose h

-- | Run the solver against all words.
problemAIWords :: IO ()
problemAIWords = do
  T.lines <$> TIO.readFile "etc/fail.log" >>=
    mapM_ (aiGameWithWord stdout) 

-- | Allow the AI solver to take guesses until the game is over.
playGameAI :: Game -> Int -> Handle -> IO ()
playGameAI g i h = do
  -- drawGrid g
  if g ^. done
    then do let t = "WORD: "<>g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
            TIO.hPutStrLn h t
            hFlush h
    else do
    ht <- hint g
    case ht of
      Nothing  -> do
        playGameAI (backtrack g) i h
      (Just t) -> do
        playGameAI (doGuess g t) (i+1) h

-- | Play a game while entering the scores manually.
feedbackGame :: IO ()
feedbackGame = playFeedbackGame emptyGame

-- | Take moves and their feedback scores until the game is done.
playFeedbackGame :: Game -> IO ()
playFeedbackGame g = runInputT defaultSettings loop
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
                     let guess = head ws
                         score = ws !! 1
                         g'    = processInfo guess score g
                     if score == "GGGGG"
                       then TIO.putStrLn $ "Done in "<>T.pack (show $ g' ^. numAttempts)<>" attempts."
                       else do
                       h <- hint g'
                       case h of
                         Nothing  -> TIO.putStrLn "No suggestions, sorry."
                         (Just t) -> do TIO.putStrLn ("Try "<>t)
                                        playFeedbackGame g'
                     else liftIO $ do
                     TIO.putStrLn "Try again."
                     playFeedbackGame g
                     
-- * Hints.

-- | Suggest some words based on the state of the game.
showHints :: Game -> IO ()
showHints g = hints g >>= mapM_ TIO.putStrLn

-- | Suggest a single word based on the state of the game.
showHint :: Game -> IO ()
showHint g = hint g >>= mapM_ TIO.putStrLn