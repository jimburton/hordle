# Hordle

A Haskell solver for [Wordle](https://www.powerlanguage.co.uk/wordle/), for
teaching purposes on the Functional Programming course at the University of
Brighton.

You can use it to play an interactive game of Wordle, and it includes
a solver which can provide hints or just play a game by itself. The
purpose is to demonstrate functional problem solving, including the
use of algebraic datatypes, higher-order functions like `foldl'`, and
the use of standard data structures like maps and sets.

The solver is simple, It is (currently) so simple that it can't solve
all Wordle problems in six guesses, which is the maximum allowed,
although it solves most in two or three. It uses a greedy backtracking
algorithm to pick the next guess, choosing one that will minimise the
subsequent possibilities but retracing its steps when that turned out
to be a bad choice.

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

The first thing we need is a way of representing this information that
can be used to filter a list of words. We create a datatype `CharInfo`,
whose constructors are named after the colours used in Wordle:

```haskell
data CharInfo = Green Int          -- ^ Char is at this index.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)
```

We will create a
[map](https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html)
from `Char`s to `CharInfo` values. The map will be updated after each
guess, and it is used when the human player asks for a hint or the
solver picks its next guess. So the map needs to be carried throughout
the game. We create a record to hold all the information needed to
play a game. The nice way to work with records is by using [lenses]()
to access and update the fields. So, we name the fields with an
underscore prefix and add the magic incantation beneath the
declaration:

```haskell
-- in Hordle.Game
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((.~), (%~), (^.))

data Game = Game
  { _word     :: Text              -- ^ The word to guess.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  } deriving (Show)

$makeLenses(''Game)
```

This means that there are now composable getters and setters for
`Game`. After creating a game, `g`, we can get one its fields with
the`(^.)` operator. For instance, 

The function that handles a new guess will need to take a
`Game` and produce a new one with an updated `_info` map.

We can see that once a character is known to be definitely not in the
target word (black), or definitely in it at a certain index that we
have guessed correctly (green), this information will never
change. When a character is marked as `Yellow` however, the positions
at which it does not appear may need to be added to. We could have
used a plain old list for te positions, but there's no point in having
duplicate entries in it so the right way to store the collection of
indices at which this character doesn't appear is as a Set. Characters
which are known to be `Yellow`, appearing somewhere but we're not sure
where, will also change to `Green` when we get more information.

The solver builds up the map of information about each character that
has appeared in a guess.
