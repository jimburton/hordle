module Hordle.Solver.Internal where

import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.List (sortBy, foldl')
import           Data.Maybe (fromJust, listToMaybe, catMaybes)
import           Hordle.Hordle
import           Hordle.Types
import           Hordle.Dict

-- | Get all hints based on the constraints. 
hints :: Game -> IO (Vector Text)
hints g = findWords (M.toList $ g ^. info) (g ^. blacklist) <$> dict

-- | Find words based on a number of constraints.
findWords :: [(Char, CharInfo)] -- ^ 
          -> [Text]             -- ^ A list of words that must not be in the result. 
          -> Vector Text        -- ^ A list of words to search.
          -> Vector Text        -- ^ The matching words.
findWords inf bl =
  V.filter (\t ->
              t `notElem` bl
              && all (\(c,pos) ->
                        case pos of
                          (Green is)  -> all (\i -> T.index t i == c) (S.elems is)
                          (Yellow os) -> T.elem c t && fromJust (T.findIndex (==c) t) `S.notMember` os
                          Black       -> not $ c `T.elem` t) inf)

-- | Take a step backward in the game.
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
                                    Black    -> M.delete d acc
                                    Yellow j -> if S.size j == 1
                                                then M.delete d acc
                                                else M.insert d (Yellow $ S.delete i j) acc
                                    Green _  -> if any (\(d',s') -> d'==d && isGreen s') (concat $ tail $ g ^. attempts)
                                                then acc
                                                else M.delete d acc) m (zip b [0..]))
      & attempts  %~ tail
      & blacklist %~ (T.pack (map fst b):)
      & guess     .~ b'
