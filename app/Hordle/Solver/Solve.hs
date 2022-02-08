{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Hordle.Solver.Solve where

import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           Data.Maybe (fromJust, listToMaybe, catMaybes)
import           Data.Functor ((<&>))
import           Data.List (sortBy, foldl')
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Hordle.Hordle
import           Hordle.Types
import           Hordle.Solver.Internal
import qualified Hordle.Solver.LookAhead as LA

-- | 
processInfo :: Text -> Text -> Game -> Game
processInfo guess target g =
  let sc = LA.score guess target in
    mapAttempt g sc & numAttempts %~ (+1)
                    & blacklist %~ (guess:)

-- | 
score :: Text  -- ^ The attempt.
      -> Map Char CharInfo  -- ^ The current knowledge.
      -> [(Char, CharInfo)] -- ^ The scored attempt, just those chars that appear in the map.
score attempt infoMap =
  catMaybes $ zipWith (\c i -> case M.lookup c infoMap of
                          (Just (Green is)) -> Just (c,Green (S.insert i is))
                          (Just (Yellow os)) -> Just (c,Yellow (S.insert i os))
                          (Just Black)       -> Just (c,Black)
                          Nothing -> Nothing) (T.unpack attempt) [0..]

-- | 
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = V.map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) $ V.toList reds'
  pure $ fst <$> listToMaybe res

-- | Apply the fixed first word for automated games.
firstGuess :: Game -> Game
firstGuess = flip doGuess firstWord

doGuess :: Game -> Text -> Game
doGuess g attempt =
  let a = score attempt (g ^. info) in
    endGame $ mapAttempt g a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt
