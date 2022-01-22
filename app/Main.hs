{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Random (getStdRandom, randomR) 
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Char (isAlpha, isAscii)
import           Data.List ((\\))

fiveLetterWords :: IO [Text]
fiveLetterWords = do dict <- map T.toUpper <$> T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
                     pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) dict

{-
do dict <- T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
                     let fiveLetterWords = filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) dict
                     pure fiveLetterWords
-}                     

main :: IO ()
main = do flw <- fiveLetterWords
          w <- (flw !!) <$> getStdRandom (randomR (0, length flw))
          print w
          loop (T.toUpper w) [] [] 0
          where loop :: Text -> [(Char, Int)] -> [(Char, Int)] -> Int -> IO ()
                loop w _     _   5 = TIO.putStrLn $ "Failed! Word was " <> w
                loop w fixed inc n = do
                  TIO.putStrLn $ "Enter a five letter word [Attempt " <> T.pack (show (n+1)) <> "]"
                  x <- T.toUpper <$> TIO.getLine
                  if w == x
                    then TIO.putStrLn "Success!"
                    else do let (d, f, i) = display w (zip (T.unpack x) [0..])
                            TIO.putStrLn (d <> "\n" <> x)
                            loop w f i (n+1)

display :: Text -> [(Char, Int)] -> (Text, [(Char, Int)], [(Char, Int)])
display target attempt = let (f,i) = doWord target attempt
                             a     = map fst attempt 
                             l1 = foldr (\x acc ->
                                           if x `elem` f
                                           then "\x2713" <> acc
                                           else if x `elem` i
                                                then "-" <> acc
                                                else "X" <> acc) "" attempt in
                           (l1, f, i)

doWord :: Text -> [(Char, Int)] -> ([(Char, Int)], [(Char, Int)])
doWord target attempt = let f  = fixed target attempt in
                          (f, inc target attempt \\ f)

fixed :: Text -> [(Char, Int)] -> [(Char, Int)]
fixed target a = map (\(cs,i) -> (fst cs, i)) $ filter (\(cs,_) -> fst cs == snd cs) $ zipWith (\c (d,i) -> ((c,d),i)) (T.unpack target) a

inc :: Text -> [(Char, Int)] -> [(Char, Int)]
inc target = filter (\(c,i) -> elem c (T.unpack target))
