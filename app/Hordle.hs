{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hordle (display, doWord) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List ((\\), sortBy)
import           Data.Char (isAlpha, isAscii)
import           Data.Maybe (fromJust)
import           Data.Bifunctor (first)
import qualified Data.Map as Map
import           Data.Map.Strict (insertWith)
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.))
import           System.Random (getStdRandom, randomR)

data CharStatus = Correct   -- ^ Char is guessed correctly
                | InWord    -- ^ Char is in the target word but wrong position
                | Incorrect -- ^ Char is not in the target word
                deriving (Show, Eq)

type Guess = [(Char, CharStatus)]

data Game = Game
  { _word     :: Text       -- ^ The word to guess        
  , _attempts :: [Guess]    -- ^ The previously guessed words
  , _guess    :: Maybe Text -- ^ The latest guess
  , _done     :: Bool       -- ^ game over flag
  } deriving (Show)

$(makeLenses ''Game)

-- | Start a new game.
initGame :: IO Game
initGame = getTarget >>= \w -> pure Game {_word=w, _attempts=[], _guess=Nothing, _done=False}

-- | A dictionary of five letter words.
dict :: IO [Text]
dict = do
  d <- map T.toUpper . T.lines <$> TIO.readFile "/etc/dictionaries-common/words"
  pure $ filter ((==5) . T.length) $ filter (T.all (\c -> isAlpha c && isAscii c)) d

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- dict
  (flw !!) <$> getStdRandom (randomR (0, length flw))
          
-- | Enter a guessed word
doGuess :: Game -> Game
doGuess g = let w = g ^. word
                z = w `T.zip` fromJust (g ^. guess)
                r = map (\(c,d) -> if c==d
                                   then (d, Correct)
                                   else if d `elem` T.unpack w
                                        then (d, InWord)
                                        else (d, Incorrect)) z in
              g & attempts %~ (r:)
                & guess    .~ Nothing

freqTable :: [Text] -> [(Char, Int)]
freqTable dict = let str = T.concat dict in
  sortBy (\e1 e2 -> snd e2 `compare` snd e1) $ Map.toList $ T.foldl
  (\acc x -> insertWith (\_ y -> y+1) x 1 acc) Map.empty str

display :: [(Char, Int)] -- ^ The attempt
        -> [(Char, Int)] -- ^ Chars in the right position and their indices
        -> [(Char, Int)] -- ^ Chars in word but in wrong position and their indices
        -> Text          -- ^ Display text
display attempt f i =
  foldr (\x acc ->
            if x `elem` f
            then "\x2713" <> acc
            else if x `elem` i
                 then "-" <> acc
                 else "X" <> acc) "" attempt

doWord :: Text                           -- ^ The target word
       -> [(Char, Int)]                  -- ^ The attempt
       -> ([(Char, Int)], [(Char, Int)]) -- ^ A pair of (chars in right position, chars in word but wrong position)
doWord target attempt = let f  = fixed target attempt in
                          (f, inc target attempt \\ f)

fixed :: Text          -- ^ The target word
      -> [(Char, Int)] -- ^ The attempt
      -> [(Char, Int)] -- ^ A list of pairs of (chars in the right position, their indices)
fixed target a = map (first fst) $ filter (\(cs,_) -> uncurry (==) cs) $ zipWith
  (\c (d,i) -> ((c,d),i)) (T.unpack target) a

inc :: Text -- The target word
    -> [(Char, Int)] -- ^ The attempt
    -> [(Char, Int)] -- ^ A list of pairs of (chars in word but not in right position, their indices)
inc target attempt = inc' target attempt []
  where inc' :: Text -> [(Char, Int)] -> [(Char, Int)] -> [(Char, Int)]
        inc' _ [] res = res
        inc' t a res  =
          if T.null t
          then res
          else let (c,i) = head a in
                 if c `elem` T.unpack t
                 then inc' (dropOne c t) (tail a) ((c,i):res)
                 else inc' t (tail a) res

dropOne :: Char -> Text -> Text
dropOne c t 
  | T.null t      = T.empty
  | c == T.head t = T.tail t
  | otherwise     = T.head t `T.cons` dropOne c (T.tail t)
