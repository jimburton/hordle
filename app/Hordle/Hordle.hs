{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections #-}
{-|
Module      : Hordle.Hordle
Description : Core library for playing Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Core library for playing Hordle.
-}
module Hordle.Hordle (
  Game(..)
  , done
  , attempts
  , numAttempts
  , success
  , word
  , guess
  , CharInfo(..)
  , emptyGame
  , initGame
  , initGameWithWord
  , doGuess
  , hint
  , hints
  , isDictWord
  , backtrack
  , targets
  , updateMapWithAttempts
  , processInfo ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List (sortBy, foldl')
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           System.Random (getStdRandom, randomR)
import           Data.Functor ((<&>))

-- * Types

data CharInfo = Green Int          -- ^ Char is at this index.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type Guess = [(Char, CharInfo)]

data Game = Game
  { _word     :: Text              -- ^ The word to guess.
  , _numAttempts :: Int            -- ^ The number of attempts.
  , _attempts :: [Guess]           -- ^ Previous attempts.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  , _guess    :: Maybe Text        -- ^ The latest guess.
  , _done     :: Bool              -- ^ game over flag.
  , _success  :: Bool              -- ^ Game was won.
  , _blacklist :: [Text]           -- ^ List of guesses that led to dead ends.
  } deriving (Show)

$(makeLenses ''Game)

-- * Constructing games

-- | Create an empty game.
emptyGame :: Game
emptyGame =
  Game {_word=""
       , _numAttempts = 0
       , _attempts=[]
       , _info    =Map.empty
       , _guess   =Nothing
       , _done    =False
       , _success =False
       , _blacklist =[]}

-- | Start a new game with a given target word.
initGameWithWord :: Text -> Game
initGameWithWord t = emptyGame & word .~ t

-- | Start a new game.
initGame :: IO Game
initGame = getTarget <&> initGameWithWord                    

-- * Playing the game

-- | Enter a guessed word into the game, updating the record accordingly.
doGuess :: Game -> Text -> Game
doGuess g attempt =
  let w = g ^. word
      x = zip (T.unpack attempt) [0..]
      a =  processAttempt w x in  
    -- update the info map. 
    endGame $ g & info %~ updateMapWithAttempts a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt

-- | Set the booleans that determine whether the game is over.
endGame :: Game -> Game
endGame g = let won = not (null $ g ^. attempts) && all (isGreen . snd) (head (g ^. attempts)) in
              g & success .~ won
                & done .~ (won || (g ^. numAttempts) == 6)
  
-- | Predicates for types of CharInfo.
isGreen, isBlack :: CharInfo -> Bool
isGreen (Green _) = True
isGreen _         = False
isBlack Black     = True
isBlack _         = False

-- | Get all hints based on the constraints.
hints :: Game -> IO [Text]
hints g = do
  let inf = Map.toList $ g ^. info
      gy  = map (\(c,i) -> case i of
                             (Green j)  -> (c,Left j)
                             (Yellow s) -> (c,Right s)) $ filter ((not . isBlack) . snd) inf
      b   = map fst $ filter (isBlack . snd) inf
  findWords gy b (g ^. blacklist) <$> targets

-- | Get a single hint based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) reds'
  pure $ fst <$> listToMaybe res

updateMapWithAttempts :: [(Char, CharInfo)] -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempts a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow si) -> Map.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Yellow o) -> Yellow (Set.union o new)
                                    -- was previously Correct, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow si) acc
               -- chars which are correct and incorrect
               s'          -> case Map.lookup d acc of
                                (Just (Green _)) -> acc
                                _                -> Map.insert d s' acc) m a

-- | Set the status of each char in a guess.
processAttempt :: Text       -- ^ The target word
       -> [(Char, Int)]      -- ^ The attempt
       -> [(Char, CharInfo)] -- ^ A pair of chars and their status in the guess.
processAttempt target attempt =
  let w' = T.unpack target
      iw = yellows target attempt in
    map (\(c, (d,i)) -> if c==d
                        then (d, Green i)
                        else if d `notElem` w'
                             then (d, Black)
                             else (d, maybeInc iw (d,i))) (zip w' attempt)

-- | Find the Yellow and Black chars in an attempt.
maybeInc :: [(Char, CharInfo)] -> (Char, Int) -> CharInfo
maybeInc [] _                      = Black
maybeInc ((c, Yellow si):iw) (d,i) = if c==d && Set.member i si
                                     then Yellow si
                                     else maybeInc iw (d,i)
maybeInc (_:iw) di                 = maybeInc iw di

-- | Find the Yellow chars in an attempt.
yellows :: Text               -- ^ The target word
        -> [(Char, Int)]      -- ^ The attempt
        -> [(Char, CharInfo)] -- ^ A list of pairs of (chars in word but not in right position, their indices)
yellows target attempt = inc' target attempt []
  where inc' :: Text -> [(Char, Int)] -> [(Char, CharInfo)] -> [(Char, CharInfo)]
        inc' _ [] res = res
        inc' t a res  =
          if T.null t
          then res
          else let (c,i) = head a in
                 if c `T.elem` t
                 then inc' (dropOne c t) (tail a) ((c, Yellow (Set.singleton i)):res)
                 else inc' t (tail a) res

-- | Drop the first instance of a Char from a Text.
dropOne :: Char -> Text -> Text
dropOne c t 
  | T.null t      = T.empty
  | c == T.head t = T.tail t
  | otherwise     = T.head t `T.cons` dropOne c (T.tail t)

-- | Update the info map in a game with an attempt.
mapAttempt :: Game -> [(Char, CharInfo)] -> Game
mapAttempt g a = g & info %~ updateMapWithAttempts a

-- | Update a game with a word and its manually entered score.
processInfo :: Text -> Text -> Game -> Game
processInfo t s g = let a = map (\((c,ci),i) -> (c, charToInfo ci i)) $ zip (T.zip t s) [0..] in
  mapAttempt g a & numAttempts %~ (+1)

-- | Convert a Char to a CharInfo value.
charToInfo :: Char -> Int -> CharInfo
charToInfo 'G' i = Green i
charToInfo 'Y' i = Yellow (Set.singleton i)
charToInfo _   _ = Black

-- | Take a step backward in the game. Used by the solver.
backtrack :: Game -> Game
backtrack g =
  case g ^. guess of
    Nothing -> g
    Just _  -> let b = if null $ g ^. attempts then [] else head $ g ^. attempts in
      endGame $ g & info %~ (\m -> foldl'
                              (\acc ((d,s),i) ->
                                  case s of -- move the info map back to previous state 
                                    Black    -> Map.delete d acc
                                    Yellow j -> if Set.size j == 1
                                                then Map.delete d acc
                                                else Map.insert d (Yellow $ Set.delete i j) acc
                                    Green _  -> if any (\(d',s') -> d'==d && isGreen s') (concat $ tail $ g ^. attempts)
                                                then acc
                                                else Map.delete d acc) m (zip b [0..]))
      & attempts  %~ tail
      & blacklist %~ (T.pack (map fst b):)
      & guess     .~ if length (g ^. attempts) > 1
                     then Just (T.pack (map fst (head (tail $ g ^. attempts))))
                     else Nothing

-- * Dictionaries

-- | A dictionary of five letter words.
dict :: IO [Text]
dict = map T.toUpper . T.lines <$> TIO.readFile "etc/long.txt"

-- | Is a word in the dictionary?
isDictWord :: Text -> IO Bool
isDictWord t = dict <&> elem t 
  
-- | A list of relatively common words to use as targets.
targets :: IO [Text]
targets = map T.toUpper . T.lines <$> TIO.readFile "etc/short.txt"

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- targets
  (flw !!) <$> getStdRandom (randomR (0, length flw))

-- | Find words based on a number of constraints.
findWords :: [(Char, Either Int (Set Int))] -- ^ Chars that are in the words, either at an exact index or not in any of a list of indices.
          -> [Char]                         -- ^ Chars that are not in the words.
          -> [Text]                         -- ^ A list of words that must not be in the result. 
          -> [Text]                         -- ^ A list of words to search.
          -> [Text]                         -- ^ The matching words.
findWords gy b bl =
  filter (\t ->
            t `notElem` bl
            && all (\(c,pos) ->
                      case pos of
                        (Left i)   -> T.index t i == c
                        (Right os) -> T.elem c t && fromJust (T.findIndex (==c) t) `Set.notMember` os) gy
            && not (any (`T.elem` t) b))
