{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Random (getStdRandom, randomR) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (isAlpha, isAscii)
import           Data.List ((\\), sortBy)
import           Data.Bifunctor (first)
import qualified Data.Map as Map
import           Data.Map.Strict (insertWith)
import           Control.Monad (when)

fiveLetterWords :: IO [Text]
fiveLetterWords = do
  dict <- map T.toUpper . T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
  pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) dict

freqTable :: [Text] -> [(Char, Int)]
freqTable dict = let str = T.concat dict in
  sortBy (\e1 e2 -> snd e2 `compare` snd e1) $ Map.toList $ T.foldl (\acc x -> insertWith (\_ y -> y+1) x 1 acc) Map.empty str

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
  let a  = zip (T.unpack attempt) [0..]
      f = fixed target a
      i = inc target a \\ f
      d = display (zip (T.unpack attempt) [0..]) f i
  TIO.putStrLn d
  if length f == 5
    then TIO.putStrLn $ "Success in " <> T.pack (show (n+1)) <> " attempts!"
    else game target (n+1)

display :: [(Char, Int)] -- The attempt
        -> [(Char, Int)] -- Chars in the right position and their indices
        -> [(Char, Int)] -- Chars in word but in wrong position and their indices
        -> Text          -- Display text
display attempt f i =
  foldr (\x acc ->
            if x `elem` f
            then "\x2713" <> acc
            else if x `elem` i
                 then "-" <> acc
                 else "X" <> acc) "" attempt

doWord :: Text                           -- The target word
       -> [(Char, Int)]                  -- The attempt
       -> ([(Char, Int)], [(Char, Int)]) -- A pair of (chars in right position, chars in word but wrong position)
doWord target attempt = let f  = fixed target attempt in
                          (f, inc target attempt \\ f)

fixed :: Text -- The target word
      -> [(Char, Int)] -- The attempt
      -> [(Char, Int)] -- A list of pairs of (chars in the right position, their indices)
fixed target a = map (first fst) $ filter (\(cs,_) -> uncurry (==) cs) $ zipWith (\c (d,i) -> ((c,d),i)) (T.unpack target) a

inc :: Text -- The target word
    -> [(Char, Int)] -- The attempt
    -> [(Char, Int)] -- A list of pairs of (chars in word but not in right position, their indices)
inc target attempt = inc' target attempt []
  where inc' :: Text -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
        inc' _ [] res = res
        inc' t a res = if T.null t
                       then res
                       else let (c,i) = head a in
                              if c `elem` T.unpack t
                              then inc' (dropOne c t) (tail a) ((c,i):res)
                              else inc' t (tail a) res

dropOne :: Char -> Text -> Text
dropOne c t 
  | T.null t      = T.empty
  | c == T.head t = T.tail t
  | otherwise     = T.head t `T.cons` dropOne c (T.tail t)
