{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-|
Module      : Hordle.Solver.Solve
Description : Solver for Hordle word games.
Maintainer  : jimburton1@gmail.com
Stability   : experimental
Portability : POSIX

The solver uses a greedy, backtracking minimax algorithm. It maintains
a "testbed" game with no knowledge of the secret word but whose info
map is kept in sync with that of the real game. Candidate words are
submitted as guesses to the testbed game, then one which yields the
fewest subsequent possibilities is chosen.
-}
module Hordle.Solver.Solve
  ( score
  , hint
  , doGuessBlind ) where

import           Lens.Micro ((&), (%~), (^.), (?~))
import           Data.Maybe (listToMaybe, catMaybes)
import           Data.Functor ((<&>))
import           Data.List (sortBy)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Hordle.Hordle as H
import           Hordle.Types
  ( Game
  , attempts
  , numAttempts
  , guess
  , info
  , blacklist
  , CharInfo(..))
import           Hordle.Solver.Internal (hints)

-- | Score an attempt against the map, in absence of the secret word.
score :: Text  -- ^ The attempt.
      -> Map Char CharInfo  -- ^ The current knowledge.
      -> [(Char, CharInfo)] -- ^ The scored attempt, just those chars that appear in the map.
score attempt infoMap =
  catMaybes $ zipWith (\c i -> case M.lookup c infoMap of
                          (Just (Green is)) -> Just (c,Green (S.insert i is))
                          (Just (Yellow os)) -> Just (c,Yellow (S.insert i os))
                          (Just Black)       -> Just (c,Black)
                          Nothing -> Nothing) (T.unpack attempt) [0..]

-- | Get a hint from the testbed game. This differs from the LookAhead hint function
-- in that it uses @doGuessBlind@ rather than @doGuess@ to pick the best hint.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = V.map (\t -> (t, doGuessBlind g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) $ V.toList reds'
  pure $ fst <$> listToMaybe res

-- | Take a guess against the testbed game. Because the testbed game does not
-- have a copy of the secret word, the feedback provided is less informative. 
doGuessBlind :: Game -- ^ The testbed game.
             -> Text -- ^ The word to guess.
             -> Game
doGuessBlind g attempt =
  let a = score attempt (g ^. info) in
    H.endGame $ g & info %~ H.updateMapWithAttempt a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt
      & blacklist %~ (attempt:)
