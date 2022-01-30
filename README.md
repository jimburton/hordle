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


