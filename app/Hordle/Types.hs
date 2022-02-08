{-# LANGUAGE TemplateHaskell #-}
{-|
Module      : Hordle.Types
Description : Types for Hordle.
Maintainer  : j.burton@brighton.ac.uk
Stability   : experimental
Portability : POSIX

Types for Hordle.
-}
module Hordle.Types
  ( CharInfo(..)
  , ScoredWord
  , Game(..)
  , word
  , numAttempts
  , attempts
  , info
  , guess
  , done
  , success
  , blacklist ) where

import Lens.Micro.TH (makeLenses)
import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

-- * Types

data CharInfo = Green (Set Int)    -- ^ Char is at these indices.
                | Yellow (Set Int) -- ^ Char is in the target word but not at any of these positions.
                | Black            -- ^ Char is not in the target word.
                deriving (Show, Eq)

type ScoredWord = [(Char, CharInfo)]

data Game = Game
  { _word     :: Text              -- ^ The word to guess.
  , _numAttempts :: Int            -- ^ The number of attempts.
  , _attempts :: [ScoredWord]      -- ^ Previous attempts.
  , _info     :: Map Char CharInfo -- ^ Info on previous guesses.
  , _guess    :: Maybe Text        -- ^ The latest guess.
  , _done     :: Bool              -- ^ game over flag.
  , _success  :: Bool              -- ^ Game was won.
  , _blacklist :: [Text]           -- ^ List of guesses that led to dead ends.
  } deriving (Show)

$(makeLenses ''Game)
