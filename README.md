# Hordle

A Haskell solver for [Wordle](https://www.powerlanguage.co.uk/wordle/), for
teaching purposes on the Functional Programming course at the University of
Brighton.

You can use it to play an interactive game of Wordle, and it includes
a solver which can provide hints or just play a game by itself. The
purpose is to demonstrate functional problem solving, including the
use of algebraic datatypes, higher-order functions like `foldl'`, and
the use of standard data structures like maps and sets.

The solver is simple but it can guess all standard Wordle words in
well under six guesses, which is the maximum allowed. It uses a greedy
backtracking algorithm to pick the next guess, choosing one that will
minimise the subsequent possibilities but retracing its steps when
that turned out to be a bad choice.

The three main ways to interact with the game are

+ run the main method to play an interactive game: `cabal run
  hordle`. Enter `:hint` to get a suggestion for your next word.
  
+ open it in the REPL to run the solver against all words. This takes
  a long time and generates a log in `etc/solver.log`:

  ```
  $ cabal repl hordle
  ghci> solveAll
  ```

+ open it in the repl and run `feedbackGame` to play a "feedback
  game". This is one in which you enter words and their score. You can
  use it to solve third party Wordle type puzzles, such as the
  original one.

## Wordle

In the game of Wordle, the player needs to guess a word within six
tries. After entering a guess the player is given feedback on it --
each character is highlighted to indicate whether it is:

+ in the target word at the same position as in the guess (green),
+ in the target word at a different position (yellow), or
+ not in the target word (black).

Wordle has attracted a great deal of attention from data scientists
who have calculated all sorts of heuristics (such as which words to
use as the best first guess) and strategies for playing the game. Many 
solvers exist.

To model the game in Haskell, the first thing we need is a way of
representing the result of a guess. We create a datatype `CharInfo`,
whose constructors are named after the colours used in Wordle. A value
of `CharInfo` is either `Green s`, where `s` is a set of ints which
are the indices in the target word that a character does appear, `Yellow s`, 
where `s` is a set of indices where a character does not appear
(the implication being that it does appear somewhere else) or `Black`,
to indicate that a character is not found in the target word.

```haskell
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S

data CharInfo = Green (Set Int)    -- ^ Char is definitely at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)
```

Now we can take an attempt and score it against a target word. We zip the characters
from the two `Text` values together, then zip that with a list of integers so we
can keep track of the index of characters.

```haskell
type Guess = [(Char, CharInfo)]

score :: Text -> Text -> Guess
score guess target = 
  map (\((c,d),i) -> if c==d 
                     then (c, Green (Set.singleton i))
                     else if T.elem c target
                          then (c, Yellow (S.singleton i))
                          else (c, Black)) $ zip (T.zip guess target) [0..]
```

Now we create a record that will store all the necessary information for
a game and can be updated throughout it. We will use lenses with
the record to make it easy to update its fields.

```haskell
import           Lens.Micro.TH (makeLenses)
import           Lens.Micro ((&), (.~), (%~), (^.), (?~))

data Game = Game
  { _word     :: Text              -- ^ The target word.
  , _numAttempts :: Int            -- ^ The number of attempts.
  , _attempts :: [Guess]           -- ^ Previous guesses.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  , _guess    :: Maybe Text        -- ^ The latest word entered.
  , _done     :: Bool              -- ^ Game over flag.
  , _success  :: Bool              -- ^ Game was won.
  } deriving (Show)

$(makeLenses ''Game)

```

To explain the lens operators, if we have a `Game` value, `g`, we
can get the value of a field with `(^.)`, e.g.  `g ^. word`. We can
set the value of a field with `(&)` and `(.~)`, e.g.  `g & word .~
"HELLO"`. We can apply a function, `f`, to a field with `(%~)`, e.g.
`g & word %~ f`. The `(?~)` operator sets a value in a field
that contains a `Maybe`.

The `_info` field is a
[map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
from `Char`s to `CharInfo` values. Each character that has appeared in
a guess will have an entry in the map, which will be updated after
each guess. It is used when the human player asks for a hint and when
the automated solver picks its next guess. The function that handles a
new guess will need to take a `Game` and produce a new one with an
updated `_info` map.

## Creating games

Some of the functions that create new games run in the IO monad,
whereas some are pure. This is becuase some of them need access to a
dictionary in order to pick a random starting word. In fact, the game
uses two dictionaries: a relatively short one for target words, and a
more complete one for checking guesses against.

```haskell
-- | A dictionary of five letter words.
dict :: IO [Text]
dict = map T.toUpper . T.lines <$> TIO.readFile "etc/long.txt"

-- | Is a word in the dictionary?
isDictWord :: Text -> IO Bool
isDictWord t = dict <&> elem t 
  
-- | A list of relatively common words to use as targets.
targets :: IO [Text]
targets = map T.toUpper . T.lines <$> TIO.readFile "etc/short.txt"

-- | Get a word to be the target for a game.
getTarget :: IO Text
getTarget = do
  flw <- targets
  (flw !!) <$> getStdRandom (randomR (0, length flw))

-- | Create an empty game.
emptyGame :: Game
emptyGame =
  Game {_word=""
       , _numAttempts = 0
       , _attempts=[]
       , _info    =Map.empty
       , _guess   =Nothing
       , _done    =False
       , _success =False}

-- | Start a new game with a given target word.
initGameWithWord :: Text -> Game
initGameWithWord t = emptyGame & word .~ t

-- | Start a new game.
initGame :: IO Game
initGame = getTarget <&> initGameWithWord
```

Now we have enough infrastructure to create a game and make a guess. We
will assume that the attempt is a real word, leaving that to the UI to check.
We also need to check whether the game has ended, i.e. whether all six guesses 
have been used or the target word was guessed correctly. This is handled by
the `endGame` function.

```haskell
-- | Enter a guessed word into the game, updating the record accordingly.
doGuess :: Game -> Text -> Game
doGuess g attempt =
  let w = g ^. word
      a = score w attempt in
    endGame $ g & info %~ updateMapWithAttempt a
	  & attempts %~ (a:)
      & numAttempts %~ (+1)
      & guess    ?~ attempt

-- | Set the booleans that determine whether the game is over.
endGame :: Game -> Game
endGame g = let won = not (null $ g ^. attempts) && all (isGreen . snd) (head (g ^. attempts)) in
              g & success .~ won
                & done .~ (won || (g ^. numAttempts) == 6)
```

The `updateMapWithAttempt` function called in `doGuess` takes a
`Guess` and the old info map then updates it. It does this using a
fold. If a character has been scored as black, we simply insert it 
-- this will overwrite the previous value if there was one, but
that will have no effect. If a character was scored as yellow, it
could be that it has previously been scored as green, so we need to
check for that. If it was previously green we keep it that way. Otherwise,
we add to the set of positions this character is not at. The opposite
is true of adding characters scored as green -- if they were
previously yellow, we overwrite the old value, otherwise we update the
set of locations.  The `insertWith` function uses its first argument to
update an existing value. Its second argument is the key. Its third
argument is used as the value if the key does not exist in the map.

```haskell
-- | Update the info map with new constraints.
updateMapWithAttempt :: Guess -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempt a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow os) -> Map.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char does not occur.
                                    (Yellow o) -> Yellow (Set.union o new)
                                    -- was previously Green, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow os) acc
               (Green is) -> Map.insertWith
                              (\(Green new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs.
                                    (Green o) -> Green (Set.union o new)
                                    -- was previously Yellow, overwrite.
                                    _         -> Green new) d (Green is) acc
               -- black.
               s'          -> Map.insert d s' acc) m a
```

Now we can actually play a game, taking guesses from the user until
the game is over. We use the `haskeline` library to make things more
convenient for the user, allowing them to use the backspace and arrow
keys when entering guesses.

```haskell
-- | Play the game by querying the user for words until they guess the word or have
-- | used their five guesses.
playGame :: Game -> IO ()
playGame g = runInputT defaultSettings loop
 where
   loop :: InputT IO ()
   loop = do
     if g ^. done
       then liftIO $ gameOver g
       else do mLn <- getInputLine ("Enter a five letter word [Attempt "
                                    <> show (1 + length (g ^. attempts))
                                    <> "]\n")
               case mLn of
                 Nothing -> loop
                 Just wdStr -> do
                   let attempt = T.toUpper $ T.pack wdStr
                     if T.length attempt /= 5
                      then liftIO $ do
                      TIO.putStrLn "Not a five letter word"
                      playGame g
                     else do
                     dw <- liftIO $ isDictWord attempt
                     if not dw
                       then liftIO $ do
                         TIO.putStrLn "Not a word from the dictionary"
                         playGame g
                       else liftIO $ do
                         let g' = doGuess g attempt
                         drawGrid g'
                         playGame g'

-- | Print a message when the game is over.
gameOver :: Game -> IO ()
gameOver g = if g ^. success
             then TIO.putStrLn "Well done!"
             else TIO.putStrLn $ "Hard luck! The word was " <> (g ^. word)
```

## Hints

So far we have created a straightforward word-guessing game. The more
interesting task is writing an automated solver for it. The first step
towards that is to find all words that can match a given set of
constraints. If we have a list of words we can filter it against the
constraints provided by a list of pairs of `Char`s and `CharInfo`
values.

```haskell
-- | Find words based on a number of constraints.
findWords :: [(Char, CharInfo)] -- ^ Chars and constraints on where they may appear in a word.
          -> [Text]             -- ^ A list of words that must not be in the result. 
          -> [Text]             -- ^ A list of words to search.
          -> [Text]             -- ^ The matching words.
findWords inf bl =
  let gy = filter (not . isBlack . snd) inf
      b  = filter (isBlack . snd) inf in
    filter (\t ->
              t `notElem` bl
             && all (\(c,pos) ->
                       case pos of
                         (Green is)  -> all (\i -> T.index t i == c) (Set.elems is)
                         (Yellow os) -> T.elem c t && fromJust (T.findIndex (==c) t) `Set.notMember` os
                         Black       -> not $ c `T.elem` t) inf)
```

We use `findWords` to get hints for a game in its current state.

```haskell
-- | Get all hints based on the constraints.
hints :: Game -> IO [Text]
hints g = findWords (Map.toList $ g ^. info) (g ^. blacklist) <$> targets
```

At the beginning of the game when there is no information available
about the target word, the hints will include the entire
dictionary. As soon as we start accumulating information that list
will be rapidly narrowed down. Each time we make a guess the feedback
we get adds more constraints, narrowing down the next set of candidate
words.

Our strategy to pick a word from the available candidates such that
the *subsequent* list of candidates will be as small as possible. So
we look at all words that match the given constraints, apply each of
them as the next guess then look at each list of resulting possible
candidates. We then take one of the words that yields the shortest
list of hints.

```haskell
-- | Get a single hint based on the constraints.
hint :: Game -> IO (Maybe Text)
hint g = do
  hs <- hints g
  let possibleGames = map (\t -> (t, doGuess g t)) hs
  reds' <- mapM (\(t,g') -> hints g' <&> (t,) . length) possibleGames
  let res = sortBy (\(_,l1) (_,l2) -> l1 `compare` l2) reds'
  pure $ fst <$> listToMaybe res
```

This is a *greedy* algorithm, because it picks a candidate word that
looks like it may be a good one given the current information, but it
could make the wrong choice. Some words will prove to be a dead end,
and we will need to retrace our steps if that happens. This
*backtracking* takes place in the function that plays an automated
game, `solveTurn`.

```haskell
-- | Start a game with a random target and a solver.
solve :: IO ()
solve = do
  g <- initGame
  solveTurn g 1 stdout

-- | Allow the AI solver to take guesses until the game is over.
solveTurn :: Game -> Int -> Handle -> IO ()
solveTurn g i h = do
  -- drawGrid g
  if g ^. done
    then do let t = "WORD: "<>g ^. word<>", SUCCESS: "<>T.pack (show $ g ^. success)<>", GUESSES: "<>T.pack (show (g ^. numAttempts))
            TIO.hPutStrLn h t
            hFlush h
    else do
    ht <- hint g
    case ht of
      Nothing  -> do
        solveTurn (backtrack g) i h
      (Just t) -> do
        solveTurn (doGuess g t) (i+1) h
```

Note that after requesting a hint, we check whether it is `Nothing`.
If that is the case, the last guess was a dead end and we need to
backtrack and try a different word.  The `backtrack` function just
undoes the changes to the `Game` record made by the previous guess.
We have added a new field to `Game`, containing a blacklist of deadend
words.

```haskell
-- | Take a step backward in the game. Used by the solver.
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
                                    Black    -> Map.delete d acc
                                    Yellow j -> if Set.size j == 1
                                                then Map.delete d acc
                                                else Map.insert d (Yellow $ Set.delete i j) acc
                                    Green _  -> if any (\(d',s') -> d'==d && isGreen s') (concat $ tail $ g ^. attempts)
                                                then acc
                                                else Map.delete d acc) m (zip b [0..]))
      & attempts  %~ tail
      & blacklist %~ (T.pack (map fst b):)
      & guess     .~ b'
```

This simple greedy backtracking algorithm isn't fast, but it can guess
any word in the 2300 list of Wordle words in less than 3 guesses on
average.
