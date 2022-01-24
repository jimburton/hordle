{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hordle (display, doWord, Game(..), dict, findWords, findWord) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List ((\\), sortBy, foldl')
import           Data.Char (isAlpha, isAscii)
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Bifunctor (first)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (insertWith)
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.))
import           System.Random (getStdRandom, randomR)

data CharStatus = Correct Int      -- ^ Char is at this index
                | InWord (Set Int) -- ^ Char is in the target word but not at any of these positions
                | Incorrect        -- ^ Char is not in the target word
                deriving (Show, Eq)

type Guess = [(Char, CharStatus)]

data Game = Game
  { _word   :: Text                -- ^ The word to guess        
  , _info   :: Map Char CharStatus -- ^ Info on previous guesses
  , _guess  :: Maybe Text          -- ^ The latest guess
  , _done   :: Bool                -- ^ game over flag
  } deriving (Show)

$(makeLenses ''Game)

-- | Start a new game.
initGame :: IO Game
initGame = getTarget >>= \w -> pure Game {_word=w, _info=Map.empty, _guess=Nothing, _done=False}

-- | Enter a guessed word
doGuess :: Game -> Game
doGuess g = let w = g ^. word
                x = fromJust (g ^. guess)
                z = zip (w `T.zip` x) [0..]
                r = map (\((c,d),i) ->
                           if c==d
                           then (d, Correct i)
                           else if d `T.elem` w && T.count (T.singleton d) x <= T.count (T.singleton d) w
                                then (d, InWord (Set.singleton i))
                                else (d, Incorrect)) z in
              g & info %~ (\m -> foldl' (\acc (d,s) -> case s of
                                            (InWord si) -> Map.insertWith (\(InWord old) (InWord new) -> InWord (Set.union old new)) d (InWord si) acc
                                            s'          -> Map.insert d s' acc) m r)
                & guess    .~ Nothing
              
-- | A dictionary of five letter words.
dict :: IO [Text]
dict = do
  d <- map T.toUpper . T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
  pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) d

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- dict
  (flw !!) <$> getStdRandom (randomR (0, length flw))

-- | Find words based on a number of constraints
findWords :: [(Char, Either Int [Int])] -- ^ Chars that are in the words, either at an exact index or not in any of a list of indices.
          -> [Char]                     -- ^ Chars that are not in the words.
          -> [Text]                     -- ^ The list of words to search.
          -> [Text]                     -- ^ The matching words.
findWords inc out =
  filter (\t ->
            all (\(c,pos) ->
                    case pos of
                      (Left i)   -> T.index t i == c
                      (Right os) -> T.elem c t && fromJust (T.findIndex (==c) t) `notElem` os) inc
            && not (any (`T.elem` t) out))

-- | Find a single word based on a number of constraints
findWord :: [(Char, Either Int [Int])] -- ^ Chars that are in the word, either at an exact index or not in any of a list of indices.
         -> [Char]                     -- ^ Chars that are not in the word.
         -> [Text]                     -- ^ The list of words to search.
         -> Maybe Text                 -- ^ The matching word, if it exists.
findWord inc out = listToMaybe . findWords inc out 

freqTable :: [Text] -> [(Char, Int)]
freqTable d = let str = T.concat d in
  sortBy (\e1 e2 -> snd e2 `compare` snd e1) $ Map.toList $ T.foldl
  (\acc x -> insertWith (\_ y -> y+1) x 1 acc) Map.empty str

display :: [(Char, Int)] -- ^ The attempt
        -> [(Char, Int)] -- ^ Chars in the right position and their indices
        -> [(Char, Int)] -- ^ Chars in word but in wrong position and their indices
        -> Text          -- ^ Display text
display attempt f i =
  foldr (\x acc ->
            if x `elem` f
            then "\x2713" <> acc
            else if x `elem` i
                 then "-" <> acc
                 else "X" <> acc) "" attempt

doWord :: Text                           -- ^ The target word
       -> [(Char, Int)]                  -- ^ The attempt
       -> ([(Char, Int)], [(Char, Int)]) -- ^ A pair of (chars in right position, chars in word but wrong position)
doWord target attempt = let f  = fixed target attempt in
                          (f, inc target attempt \\ f)

fixed :: Text          -- ^ The target word
      -> [(Char, Int)] -- ^ The attempt
      -> [(Char, Int)] -- ^ A list of pairs of (chars in the right position, their indices)
fixed target a = map (first fst) $ filter (\(cs,_) -> uncurry (==) cs) $ zipWith
  (\c (d,i) -> ((c,d),i)) (T.unpack target) a

inc :: Text -- The target word
    -> [(Char, Int)] -- ^ The attempt
    -> [(Char, Int)] -- ^ A list of pairs of (chars in word but not in right position, their indices)
inc target attempt = inc' target attempt []
  where inc' :: Text -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
        inc' _ [] res = res
        inc' t a res  =
          if T.null t
          then res
          else let (c,i) = head a in
                 if c `T.elem` t
                 then inc' (dropOne c t) (tail a) ((c,i):res)
                 else inc' t (tail a) res

dropOne :: Char -> Text -> Text
dropOne c t 
  | T.null t      = T.empty
  | c == T.head t = T.tail t
  | otherwise     = T.head t `T.cons` dropOne c (T.tail t)
