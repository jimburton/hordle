# Hordle

A Haskell solver for [Wordle](https://www.powerlanguage.co.uk/wordle/), for
teaching purposes on the Functional Programming course at the University of
Brighton.

You can use it to play an interactive game of Wordle, and it includes a 
solver which can provide hints or just play a game by itself. The purpose
is to demonstrate functional problem solving, including the use of algebraic 
datatypes, higher-order functions and so on.

The solver is simple, It is (currently) so simple that it can't solve all
Wordle problems. It uses a greedy backtracking algorithm to pick the next guess,
choosing one that will minimise the subsequent possibilities but retracing its 
steps when that turned out to be a bad choice.

## Wordle

In the game of Wordle, the player needs to guess a word within six
tries. After entering a guess the player is given feedback on it -- each character
is highlighted to indicate whether it is:

+ in the target word at the same position as in the guess,
+ in the target word at a different position, or
+ not in the target word.

This information is used to refine the guesses. The first thing we
need is a way of representing this information that can be used to
filer a list of words. We create a datatype `CharInfo`, whose
constructors are named after the colours used in Wordle:

```haskell
data CharInfo = Green Int          -- ^ Char is at this index.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)
```

We will then create a map from `Char`s to `CharInfo` values. This map will
be updated after each guess.

The solver builds up a
map of information about each character that has appeared in a guess
