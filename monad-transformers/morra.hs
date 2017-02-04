module Morra where

import Control.Monad.Trans.State

data Score = Score Integer Integer deriving (Eq, Show)

data Guess = Odd | Even deriving (Eq, Show)

data Player = One | Two deriving (Eq, Show)
data Move = Move Player deriving (Eq, Show)

type GameStep = IO (Guess, Move)

move :: (Guess, Move) -> Score -> Score
move (guess, Move which) (Score s1 s2)
  | which == One = Score (s1 + 1) s2
  | otherwise    = Score s1 (s2 + 1)

advance step = do
  s <- get
  put $ move (Odd, Move One) s
