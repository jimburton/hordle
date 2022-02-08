{-# LANGUAGE TupleSections #-}
module Hordle.Solver.LookAhead
  where

import           Data.Text (Text)
import qualified Data.Vector as V
import           Data.Maybe (listToMaybe)
import           Data.List (sortBy)
import           Data.Functor ((<&>))
import           Hordle.Hordle (doGuess)
import           Hordle.Types (Game)
import qualified Hordle.Solver.Internal as HSI

-- | Get a hint by looking ahead based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- HSI.hints g
  let possibleGames = V.map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> HSI.hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) $ V.toList reds'
  pure $ fst <$> listToMaybe res
