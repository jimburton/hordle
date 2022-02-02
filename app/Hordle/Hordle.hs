{-# LANGUAGE TemplateHaskell, OverloadedStrings, TupleSections, MultiWayIf #-}
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
  , processInfo
  , score
  ) where

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
import Debug.Trace

-- * Types

data CharInfo = Green (Set Int)    -- ^ Char is at these indices.
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
      a = score attempt w in
    endGame $ g & info %~ updateMapWithAttempt a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt

-- | Set the status of each char in a guess.
score :: Text  -- ^ The attempt.
      -> Text  -- ^ The target word.
      -> Guess -- ^ The scored attempt.
score attempt target = 
  map (\((c,d),i) -> if c==d 
                     then (c, Green (Set.singleton i))
                     else if T.elem c target
                          then (c, Yellow (Set.singleton i))
                          else (c, Black)) $ zip (T.zip attempt target) [0..]

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
hints g = findWords (Map.toList $ g ^. info) (g ^. blacklist) <$> targets

-- | Get a single hint based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) reds'
  pure $ fst <$> listToMaybe res

-- | Find words based on a number of constraints.
findWords :: [(Char, CharInfo)] -- ^ Chars that are in the words, either at an exact index or not in any of a list of indices.
          -> [Text]                               -- ^ A list of words that must not be in the result. 
          -> [Text]                               -- ^ A list of words to search.
          -> [Text]                               -- ^ The matching words.
findWords inf bl =
  filter (\t ->
             t `notElem` bl
             && all (\(c,pos) ->
                       case pos of
                         (Green is)  -> all (\i -> T.index t i == c) (Set.elems is)
                         (Yellow os) -> T.elem c t && fromJust (T.findIndex (==c) t) `Set.notMember` os
                         Black       -> not $ c `T.elem` t) inf)

-- | Update the info map with new constraints.
updateMapWithAttempt :: Guess -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempt a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow os) -> Map.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Yellow o) -> Yellow (Set.union o new)
                                    -- was previously Green, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow os) acc
               (Green is) -> Map.insertWith
                              (\(Green new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Green o) -> Green (Set.union o new)
                                    -- was previously Yellow, overwrite.
                                    _         -> Green new) d (Green is) acc
               -- chars which are incorrect
               s'          -> Map.insert d s' acc) m a

-- | Update the info map in a game with an attempt.
mapAttempt :: Game -> Guess -> Game
mapAttempt g a = g & info %~ updateMapWithAttempt a

-- | Update a game with a word and its manually entered score.
processInfo :: Text -> Text -> Game -> Game
processInfo t s g = let a = map (\((c,ci),i) -> (c, charToInfo ci i)) $ zip (T.zip t s) [0..] in
  mapAttempt g a & numAttempts %~ (+1)

-- | Convert a Char to a CharInfo value.
charToInfo :: Char -> Int -> CharInfo
charToInfo 'G' i = Green (Set.singleton i)
charToInfo 'Y' i = Yellow (Set.singleton i)
charToInfo _   _ = Black

-- | Take a step backward in the game. Used by the solver.
backtrack :: Game -> Game
backtrack g =
  case g ^. guess of
    Nothing -> g
    Just _  -> let b  = if null $ g ^. attempts then [] else head $ g ^. attempts
                   b' = if length (g ^. attempts) > 1
                        then Just (T.pack (map fst (head (tail $ g ^. attempts))))
                        else Nothing in
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
      & guess     .~ b'

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
