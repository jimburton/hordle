{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Random (getStdRandom, randomR) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (isAlpha, isAscii)
import           Data.List ((\\))
import           Data.Bifunctor (first)
import           Control.Monad (when)

fiveLetterWords :: IO [Text]
fiveLetterWords = do
  dict <- map T.toUpper . T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
  pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) dict

main :: IO ()
main = do flw <- fiveLetterWords
          w <- (flw !!) <$> getStdRandom (randomR (0, length flw))
          print w
          TIO.putStrLn "\x2713 = char in right place, - = char in word but wrong place, X = char not in word."
          game (T.toUpper w) [] [] 0

game :: Text -> [(Char, Int)] -> [(Char, Int)] -> Int -> IO ()
game w _     _   5 = TIO.putStrLn $ "Failed! Word was " <> w
game w fixed inc n = do
  TIO.putStrLn $ "Enter a five letter word [Attempt " <> T.pack (show (n+1)) <> "]"
  x <- T.toUpper . T.take 5 <$> TIO.getLine
  flw <- fiveLetterWords
  when (x `notElem` flw) (do TIO.putStrLn $ "Not a word: " <> x
                             game w fixed inc (n+1))
  let (d, f, i) = display w (zip (T.unpack x) [0..])
  TIO.putStrLn (d <> "\n" <> x)
  if length f == 5
    then TIO.putStrLn $ "Success in " <> T.pack (show (n+1)) <> " attempts!"
    else game w f i (n+1)

display :: Text                                 -- The target word
        -> [(Char, Int)]                        -- The attempt
        -> (Text, [(Char, Int)], [(Char, Int)]) -- A triple of (display text, chars in right position, chars in word but wrong position)
display target attempt = let (f,i) = doWord target attempt
                             a     = map fst attempt 
                             l1 = foldr (\x acc ->
                                           if x `elem` f
                                           then "\x2713" <> acc
                                           else if x `elem` i
                                                then "-" <> acc
                                                else "X" <> acc) "" attempt in
                           (l1, f, i)

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
-- inc target = filter (\(c,i) -> c `elem` T.unpack target)
inc target attempt = inc' target attempt []
  where inc' t [] res = res
        inc' t a res = if T.null t
                       then res
                       else let (c,i) = head a in
                              if c `elem` T.unpack t
                              then inc' (dropOne c t) (tail a) ((c,i):res)
                              else inc' t (tail a) res
        dropOne c t = if T.null t
                      then T.empty
                      else if c == T.head t
                           then T.tail t
                           else (T.head t) `T.cons` (dropOne c (T.tail t) )
