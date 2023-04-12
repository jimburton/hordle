{-|
Module      : Hordle.Dict
Description : Dictionaries.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

Dictionaries for the Hordle game. Matching the original game, there are two -- a short
one, @targets@, which contains 2300 words which may be the secret word in a game, and a
longer one, @dict@, which is a superset of @targets. It contains 12K words from which
guesses must be chosen.
-}
module Hordle.Dict
  ( dict
  , targets
  , getTarget
  , isDictWord ) where

import           Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Functor ((<&>))
import           System.Random (getStdRandom, randomR)

-- * Dictionaries

-- | Read the contents of a file into a vector or words.
filepathToDict :: FilePath -> IO (Vector Text)
filepathToDict fp = V.map T.toUpper . V.fromList . T.lines <$> TIO.readFile fp
  
-- | A dictionary of five letter words.
dict :: IO (Vector Text)
dict = filepathToDict "etc/long.txt"

-- | A list of relatively common words to use as targets.
targets :: IO (Vector Text)
targets = filepathToDict "etc/short.txt"

-- | Is a word in the dictionary?
isDictWord :: Text -> IO Bool
isDictWord t = dict <&> elem t 

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- targets
  getStdRandom (randomR (0, length flw)) <&> (V.!) flw
