{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Hordle ( Game(..)
              , done
              , attempts
              , success
              , word
              , guess
              , CharInfo(..)
              , initGame
              , initGameWithWord
              , doGuess
              , hint
              , hints
              , isDictWord
              , backtrack ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.List ((\\), sortBy, foldl')
import           Data.Char (isAlpha, isAscii)
import           Data.Maybe (fromJust, listToMaybe)
import           Data.Bifunctor (first)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map.Strict (insertWith)
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))
import           System.Random (getStdRandom, randomR)
import           Data.Functor ((<&>))
import           Debug.Trace

data CharInfo = Green Int          -- ^ Char is at this index.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type Guess = [(Char, CharInfo)]

data Game = Game
  { _word     :: Text              -- ^ The word to guess.
  , _attempts :: [Guess]           -- ^ Previous attempts.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  , _guess    :: Maybe Text        -- ^ The latest guess.
  , _done     :: Bool              -- ^ game over flag.
  , _success  :: Bool              -- ^ Game was won.
  , _blacklist :: [Text]           -- ^ List of guesses that led to dead ends.
  } deriving (Show)

$(makeLenses ''Game)

-- | Start a new game.
initGame :: IO Game
initGame = getTarget >>= \w ->
  pure Game {_word=w
            , _attempts=[]
            , _info    =Map.empty
            , _guess   =Nothing
            , _done    =False
            , _success =False
            , _blacklist =[]}

initGameWithWord :: Text -> Game
initGameWithWord t = Game {_word=t
                          , _attempts=[]
                          , _info    =Map.empty
                          , _guess   =Nothing
                          , _done    =False
                          , _success =False
                          , _blacklist =[]}

backtrack :: Game -> Game
backtrack g = case g ^. guess of
                Nothing -> g
                Just t  -> let b = head $ g ^. attempts in
                  g & info %~ (\m -> foldl' (\acc (d,s) -> Map.delete d acc) m b) -- delete everything relating to this attempt. Too much!
                  & attempts  %~ (\as -> tail as)
                  & blacklist %~ (\bs -> T.pack (map fst b):bs)
                  & guess     .~ if length (g ^. attempts) > 1
                                 then Just (T.pack (map fst (head (tail $ g ^. attempts))))
                                 else Nothing
                    
  
-- | Enter a guessed word into the game, updating the record accordingly.
doGuess :: Game -> Text -> Game
doGuess g attempt =
  let w = g ^. word
      x = zip (T.unpack attempt) [0..]
      a =  processAttempt w x in  
    -- update the info map. 
    endGame $ g & info %~ (\m ->
                             foldl' (\acc (d,s) ->
                                       case s of
                                         (Yellow si) -> Map.insertWith
                                           (\(Yellow new) old ->
                                              case old of
                                                -- update the set of indices in which this char occurs
                                                (Yellow o) -> Yellow (Set.union o new)
                                                -- was previously Correct, keep it that way and ignore the new info.
                                                o'         -> o') d (Yellow si) acc
                                         -- chars which are correct and incorrect
                                         s'          -> case Map.lookup d acc of
                                                          (Just (Green _)) -> acc
                                                          _                  -> Map.insert d s' acc) m a)
      & attempts %~ (a:)
      & guess    ?~ attempt

endGame :: Game -> Game
endGame g = let won = all (isGreen . snd) (head (g ^. attempts)) in
              g & success .~ won
                & done .~ (won || length (g ^. attempts) == 6)
  
-- | Predicates for types of CharInfo.
isGreen, isBlack :: CharInfo -> Bool
isGreen (Green _) = True
isGreen _         = False
isBlack Black     = True
isBlack _         = False

-- | Get all hints based on the constraints.
hints :: Game -> IO [Text]
hints g = do
  let inf = Map.toList $ g ^. info
      inc = map (\(c,i) -> case i of
                             (Green j) -> (c,Left j)
                             (Yellow s)  -> (c,Right s)) $ filter ((not . isBlack) . snd) inf
      out = map fst $ filter (\(c,i) -> isBlack i) inf
  findWords inc out (g ^. blacklist) <$> targets

-- | Get a single hint based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> do hs <- hints g'
                               pure (t, length hs)) possibleGames
  let res = sortBy (\(t1,l1) (t2,l2) -> l1 `compare` l2) reds'
  pure $ fst <$> listToMaybe res
              
-- | A dictionary of five letter words.
dict :: IO [Text]
dict = T.lines <$> TIO.readFile "etc/dict.txt"

-- | Is a word in the dictionary?
isDictWord :: Text -> IO Bool
isDictWord t = dict <&> elem t 
  
-- | A list of relatively common words to use as targets.
targets :: IO [Text]
targets = T.lines <$> TIO.readFile "etc/starting.txt"

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- targets
  (flw !!) <$> getStdRandom (randomR (0, length flw))

-- | Find words based on a number of constraints
findWords :: [(Char, Either Int (Set Int))] -- ^ Chars that are in the words, either at an exact index or not in any of a list of indices.
          -> [Char]                     -- ^ Chars that are not in the words.
          -> [Text]                     -- ^ A list of words that must not be in the result. 
          -> [Text]                     -- ^ A list of words to search.
          -> [Text]                     -- ^ The matching words.
findWords inc out bl =
  filter (\t ->
            t `notElem` bl
            && all (\(c,pos) ->
                      case pos of
                        (Left i)   -> T.index t i == c
                        (Right os) -> T.elem c t && fromJust (T.findIndex (==c) t) `Set.notMember` os) inc
            && not (any (`T.elem` t) out))

{-
-- | Find a single word based on a number of constraints
findWord :: [(Char, Either Int (Set Int))] -- ^ Chars that are in the word, either at an exact index or not in any of a list of indices.
         -> [Char]                     -- ^ Chars that are not in the word.
         -> [Text]                     -- ^ The list of words to search.
         -> Maybe Text                 -- ^ The matching word, if it exists.
findWord inc out = listToMaybe . findWords inc out 
-}

freqTable :: [Text] -> [(Char, Int)]
freqTable d = let str = T.concat d in
  sortBy (\e1 e2 -> snd e2 `compare` snd e1) $ Map.toList $ T.foldl
  (\acc x -> insertWith (\_ y -> y+1) x 1 acc) Map.empty str

-- | Set the status of each char in a guess.
processAttempt :: Text       -- ^ The target word
       -> [(Char, Int)]      -- ^ The attempt
       -> [(Char, CharInfo)] -- ^ A pair of chars and their status in the guess.
processAttempt target attempt =
  let w' = T.unpack target
      iw = inc target attempt in
    map (\(c, (d,i)) -> if c==d
                        then (d, Green i)
                        else if d `notElem` w'
                             then (d, Black)
                             else (d, maybeInc iw (d,i))) (zip w' attempt)

maybeInc :: [(Char, CharInfo)] -> (Char, Int) -> CharInfo
maybeInc [] _                      = Black
maybeInc ((c, Yellow si):iw) (d,i) = if c==d && Set.member i si
                                     then Yellow si
                                     else maybeInc iw (d,i)
maybeInc (_:iw) di                 = maybeInc iw di

inc :: Text -- ^ The target word
    -> [(Char, Int)] -- ^ The attempt
    -> [(Char, CharInfo)] -- ^ A list of pairs of (chars in word but not in right position, their indices)
inc target attempt = inc' target attempt []
  where inc' :: Text -> [(Char, Int)] -> [(Char, CharInfo)] -> [(Char, CharInfo)]
        inc' _ [] res = res
        inc' t a res  =
          if T.null t
          then res
          else let (c,i) = head a in
                 if c `T.elem` t
                 then inc' (dropOne c t) (tail a) ((c, Yellow (Set.singleton i)):res)
                 else inc' t (tail a) res

dropOne :: Char -> Text -> Text
dropOne c t 
  | T.null t      = T.empty
  | c == T.head t = T.tail t
  | otherwise     = T.head t `T.cons` dropOne c (T.tail t)
