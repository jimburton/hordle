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

doGuess :: Game -> Text -> Game
doGuess g attempt =
  let w = g ^. word
      a = score attempt w in
    endGame $ mapAttempt g a
      & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt

-- | Apply the fixed first word for automated games.
firstGuess :: Game -> Game
firstGuess = flip doGuess firstWord
