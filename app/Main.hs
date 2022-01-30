{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Lens.Micro ((^.))
import           System.Console.Haskeline
import           Control.Monad.IO.Class (liftIO)

import           Hordle (Game
                        , done
                        , attempts
                        , word
                        , initGame
                        , initGameWithWord
                        , doGuess
                        , hints
                        , hint
                        , isDictWord
                        , backtrack
                        , guess
                        , targets )
import           UI (drawGrid
                    , gameOver
                    , helpText)

main :: IO ()
main = do TIO.putStrLn helpText
          g <- initGame
          -- TIO.putStrLn (g ^. word)
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

-- | STart a game with a random target and AI solver.
aiGame :: IO ()
aiGame = do
  g <- initGame
  TIO.putStrLn (g ^. word)
  playGameAI g 1

-- | Start a game with a given word and AI solver.
aiGameWithWord :: Text -> IO ()
aiGameWithWord w = playGameAI (initGameWithWord w) 1

allAIWords = do
  targets >>= mapM_ aiGameWithWord

-- | Allow the AI solver to take guesses until the game is over.
playGameAI :: Game -> Int -> IO ()
playGameAI g i = do
  if g ^. done
    then gameOver g
    else do
    h <- hint g
    case h of
      Nothing  -> do
        let gs = case g ^. guess of
                   Nothing  -> ""
                   (Just t) -> t
        TIO.putStrLn $ "No hints for ["<>gs<> "]. Backtracking."
        playGameAI (backtrack g) (i-1)
      (Just t) -> do
        TIO.putStrLn $ "Guess " <>T.pack (show i)<>": "<>t
        playGameAI (doGuess g t) (i+1)

-- | Suggest some words based on the state of the game.
showHints :: Game -> IO ()
showHints g = hints g >>= mapM_ TIO.putStrLn

-- | Suggest a single word based on the state of the game.
showHint :: Game -> IO ()
showHint g = hint g >>= mapM_ TIO.putStrLn
