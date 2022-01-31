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

## Wordle

In the game of Wordle, the player needs to guess a word within six
tries. After entering a guess the player is given feedback on it --
each character is highlighted to indicate whether it is:

+ in the target word at the same position as in the guess (green),
+ in the target word at a different position (yellow), or
+ not in the target word (black).

Here is a game being played. Characters not in the target are shown in 
red, since black characters don't show up well in my terminal.

GIF

Using hints like this is cheating, obviously, but whether the human or
the solver is doing the work the feedback is used to refine the
guesses.

Wordle has attracted a great deal of attention from data scientists,
who have calculated all sorts of heuristics (such as which words to
use as the best first guess) and strategies for playing the game. 
Solvers exist which can solve any game in an average of 3.4 guesses. 
Note that, unlike ours, these depend on an expensively pre-computed
decision tree which provides the next best guess in any scenario.

To model the game in Haskell, the first thing we need is a way of
representing this information that can be used to filter a list of
words. We create a datatype `CharInfo`, whose constructors are named
after the colours used in Wordle. As usual, we import the `Data.Text`
module with a qualified name, `T`, since it contains many functions
whose names clash with those in the `Prelude` but import the name of
the constructor directly for convenience.  We do the same for
`Data.Set`.

```haskell
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Set (Set)
import qualified Data.Set as S

data CharInfo = Green (Set Int)    -- ^ Char is definitely at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)
				
type Guess = [(Char, CharInfo)]
```

Now we can take a guess and score it against a target word. We zip the characters
from the two `Text` values together, then zip that with a list of integers so we
can keep track of the index of characters.

```haskell
score :: Text -> Text -> Guess
score guess target = 
  map (\((c,d),i) -> if c==d 
                     then (c, Green (Set.singleton i))
					 else if T.elem c target
						  then (c, Yellow (S.singleton i))
						  else (c, Black)) $ zip (T.zip guess target) [0..]
```
Running it in `ghci`:

```
ghci> :set -XOverloadedStrings
ghci> score "HOLLY" "LILTS"
[('H',Black),('O',Black),('L',Green 2),('L',Yellow (fromList [3])),('Y',Black)]
```

Now we need a record that will store all the necessary information for
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

Now if we have a `Game` value, `g`, we can get the value of a field with `(^.)`, e.g.
`g ^. word`, we can set the value of a field with `(&)` and `(.~)`, e.g. 
`g & word .~ "HELLO"`, and we can apply a function, `f`, to a field with `(%~)`, e.g.
`g & word %~ T.toUpper`. The `(?~)` operator sets a value in a field that contains a
`Maybe`.

The `_info` field is a
[map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
from `Char`s to `CharInfo` values. The map will be updated after each
guess. It is used when the human player asks for a hint, or the
automated solver picks its next guess. The function that handles a new
guess will need to take a `Game` and produce a new one with an updated
`_info` map.

## Creating games

The game needs to use a dictionary to find random target words and to check that
every attempt is a real word. In fact, the game uses two disctionaries: a relatively
short one for target words, and a more complete one for checking guesses against.
Creating a game with a random target word requires IO. We also want to be able to 
create games based on predetermined words.

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
`Guess` and the old map then updates it. It does this using a fold. If
a character has been scored as black, we just insert it -- if it was
already there this will overwrite the previous value, but that will
have no effect. If a character was scored as yellow, it could be that
it has previously been scored as green, so we need to check that. If
it was previously green we keep it that way. Otherwise, we add to the
set of positions this character is not at. The opposite is true of
adding characters scored as green -- if they were previously yellow,
we overwrite the old value, otherwise we update the set of location.
The `insertWith` function uses its first argument to update an
existing value. Its second argument is the key. Its third argument is
used as the value if the key does not exist in the map.

```haskell
-- | Update the info map with new constraints.
updateMapWithAttempt :: Guess -> Map Char CharInfo -> Map Char CharInfo
updateMapWithAttempt a m =
  foldl' (\acc (d,s) ->
             case s of
               (Yellow os) -> Map.insertWith
                              (\(Yellow new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Yellow o) -> Yellow (Set.union o new)
                                    -- was previously Green, keep it that way and ignore the new info.
                                    o'         -> o') d (Yellow os) acc
               (Green is) -> Map.insertWith
                              (\(Green new) old ->
                                  case old of
                                    -- update the set of indices in which this char occurs
                                    (Green o) -> Green (Set.union o new)
                                    -- was previously Yellow, overwrite.
                                    _         -> Green new) d (Green is) acc
               -- chars which are incorrect
               s'          -> Map.insert d s' acc) m a
```

Now we can actually play a game, taking guesses from the user until
the game is over. We use the `haskeline` library to make it more
convenient to use the terminal to enter guesses.

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

So far we have created a simple word-guessing game. The more
interesting task is writing an automated solver for the game. The
first step towards that is to find all words that can match a given
set of constraints. If we have a list of words we can filter it
against the constraint provided by a pair of a `Char` and a `CharInfo`
value.

```haskell
findWords :: Guess -> [Text] -> [Text]
findWords (c,ci) ts = let f = case ci of
                                (Green i)  -> \t -> T.index t i == c
								(Yellow i) -> \t -> T.elem c t && fromJust (T.findIndex (==c) t) `S.notMember` i
								(Black)    -> \t -> not $ T.elem c t in
	filter f ts
```
The `f` function chooses the right function to apply depending on the constraint. 
If we have a list of constraints we can apply them all to each word by mapping 
`f` onto the list of constraints and using `all` to apply them all to each word.

```haskell
findWords :: [Guess] -> [Text] -> [Text]
findWords cs ts = let f  = case ci of
                            (Green i)  -> \t -> T.index t i == c
							(Yellow i) -> \t -> T.elem c t && fromJust (T.findIndex (==c) t) `S.notMember` i
							(Black)    -> \t -> not $ T.elem c t 
	                  fs = map f cs in
	filter (all fs) ts
```

At the beginning of the game when there are no constraints, the hints
will include the entire dictionary. As soon as we start accumulating
information that list will be rapidly narrowed down. Each time we make
a guess the feedback we get adds more constraints, narrowing down the
next set of candidate words.

Our strategy to pick a word from the available candidates such that
the *subsequent* list of candidates will be as small as possible. So
we look at all words that match the given constraints, apply each of
them as the next guess then look at each list of resulting possible
candidates. We then take one of the words with the shortest list.

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
and we will need to retrace our steps if that happens. This takes
place in thefunction that plays an automated game, `solveTurn`.

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
      & guess     .~ if length (g ^. attempts) > 1
                     then Just (T.pack (map fst (head (tail $ g ^. attempts))))
                     else Nothing
```

This simple backtracking greedy algorithm isn't fast, but it can guess
any word in the 2300 list of Wordle words in less than 3 guesses on
average.
