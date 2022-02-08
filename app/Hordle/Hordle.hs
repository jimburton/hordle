{-# LANGUAGE OverloadedStrings #-}
module Hordle.Hordle
  ( endGame
  , doGuess
  , isGreen
  , score
  , updateMapWithAttempt
  , mapAttempt
  , emptyGame
  , initGame
  , initGameWithWord
  , firstGuess ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (foldl')
import           Data.Functor ((<&>))
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           Hordle.Types
  (Game(..)
  , success
  , attempts
  , numAttempts
  , info
  , word
  , guess
  , CharInfo(..)
  , ScoredWord)
import           Hordle.Dict (getTarget)

-- | Set the booleans that determine whether the game is over.
endGame :: Game -> Game
endGame g = let won = not (null $ g ^. attempts) && all (isGreen . snd) (head (g ^. attempts)) in
              g & success .~ won
-- | Predicates for types of CharInfo.
isGreen :: CharInfo -> Bool
isGreen (Green _) = True
isGreen _         = False

-- | Update the info map with new constraints.
updateMapWithAttempt :: ScoredWord -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempt a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow os) -> M.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Yellow o) -> Yellow (S.union o new)
                                    -- was previously Green, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow os) acc
               (Green is) -> M.insertWith
                              (\(Green new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Green o) -> Green (S.union o new)
                                    -- was previously Yellow, overwrite.
                                    _         -> Green new) d (Green is) acc
               -- chars which are incorrect
               s'          -> M.insert d s' acc) m a

-- | Update the info map in a game with an attempt.
mapAttempt :: Game -> ScoredWord -> Game
mapAttempt g a = g & info %~ updateMapWithAttempt a

-- | Best starting word? 
firstWord :: Text
firstWord = "SOARE"

doGuess :: Game -> Text -> Game
doGuess g attempt =
  let w = g ^. word
      a = score attempt w in
    endGame $ mapAttempt g a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt

-- | Set the status of each char in a guess.
score :: Text  -- ^ The attempt.
      -> Text  -- ^ The target word.
      -> ScoredWord -- ^ The scored attempt.
score attempt target = 
  zipWith (\(c,d) i -> if c==d 
                       then (c, Green (S.singleton i))
                       else if T.elem c target
                            then (c, Yellow (S.singleton i))
                            else (c, Black)) (T.zip attempt target) [0..]


-- | Apply the fixed first word for automated games.
firstGuess :: Game -> Game
firstGuess = flip doGuess firstWord

-- * Constructing games

-- | Create an empty game.
emptyGame :: Game
emptyGame =
  Game {_word=""
       , _numAttempts = 0
       , _attempts=[]
       , _info    =M.empty
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
