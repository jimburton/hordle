{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Time (UTCTime, diffUTCTime)

data LogEntry = LogEntry {
  word :: Text
  , success :: Bool
  , numTries :: Int
  }
  deriving (Eq, Show)

main = do
  ls <- T.lines <$> TIO.readFile "etc/solver.log"
  let start = read (T.unpack $ head ls) :: UTCTime
      end   = read (T.unpack $ last ls) :: UTCTime
      diff  = diffUTCTime end start
      les = map lineToLE $ take (length ls - 2) (tail ls)
      succ = filter success les
      fail = filter (not . success) les
  -- mapM_ print les
  TIO.putStrLn "::::::::::::::::::::::::::::"
  TIO.putStrLn $ "Solver took "<>T.pack (show diff)
  TIO.putStrLn $ "Success count: "<>T.pack (show $ length succ)<>" Failure count: "<>T.pack (show $ length fail)
  TIO.putStrLn $ T.pack (show (100 - (fromIntegral (length fail) / fromIntegral (length succ)) * 100)) <> "% success"
  TIO.putStrLn "::::::::::::::::::::::::::::"
  TIO.putStrLn "Failures:"
  mapM_ print fail
  TIO.putStrLn "Average guesses when successful:"
  print (fromIntegral (foldl (\acc le -> numTries le + acc) 0 succ) / fromIntegral (length succ))
  

lineToLE :: Text -> LogEntry
lineToLE t = let ps = T.split (==',') t
                 w  = T.split (==':') (head ps) !! 1
                 s  = (read $ T.unpack $ T.split (==':') (ps !! 1) !! 1) :: Bool
                 n  = (read $ T.unpack $ T.split (==':') (ps !! 2) !! 1) :: Int in
               LogEntry w s n
