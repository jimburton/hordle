{-# LANGUAGE TupleSections #-}
module Hordle.Solver.LookAhead
  where

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Vector as V
import           Data.Maybe (fromJust, listToMaybe, catMaybes)
import           Data.List (sortBy, foldl')
import           Data.Functor ((<&>))
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           Hordle.Hordle
import           Hordle.Types
import qualified Hordle.Solver.Internal as HSI

-- | Get a hint by looking ahead based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- HSI.hints g
  let possibleGames = V.map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> HSI.hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) $ V.toList reds'
  pure $ fst <$> listToMaybe res
