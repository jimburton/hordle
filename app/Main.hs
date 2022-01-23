{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Random (getStdRandom, randomR) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (isAlpha, isAscii)
import           Control.Monad (when)
import           Hordle (display, doWord)
import           UI

fiveLetterWords :: IO [Text]
fiveLetterWords = do
  dict <- map T.toUpper . T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
  pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) dict

helpText :: Text
helpText = "\x2713 = char in right place. \n \
           \- = char in word but wrong place. \n \
           \X = char not in word. "
           
main :: IO ()
main = do flw <- fiveLetterWords
          w <- (flw !!) <$> getStdRandom (randomR (0, length flw))
          print w
          TIO.putStrLn helpText
          game (T.toUpper w) 0

game :: Text -> Int -> IO ()
game target 5 = TIO.putStrLn $ "Failed! Word was " <> target
game target n = do
  TIO.putStrLn $ "Enter a five letter word [Attempt " <> T.pack (show (n+1)) <> "]"
  attempt <- T.toUpper . T.take 5 <$> TIO.getLine
  flw <- fiveLetterWords
  when (attempt `notElem` flw) (do TIO.putStrLn $ "Not a word: " <> attempt
                                   game target (n+1))
  let a     = zip (T.unpack attempt) [0..]
      (f,i) = doWord target a
  TIO.putStrLn $ display (zip (T.unpack attempt) [0..]) f i
  if length f == 5
    then TIO.putStrLn $ "Success in " <> T.pack (show (n+1)) <> " attempts!"
    else game target (n+1)
