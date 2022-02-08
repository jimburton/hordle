{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Hordle.Hordle where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.List (sortBy, foldl')
import           Data.Functor ((<&>))
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           Hordle.Types
import           Hordle.Dict

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
