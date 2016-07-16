module Multiply where

import Data.List

multiply :: (Integral a) => a -> a -> a
multiply a b = go a b
  where go c s
           | c == 0 = 0
           | c == 1 = s
           | c == (-1) = (-s)
           | c < 0 = go (c + 1) (s + b)
           | otherwise = go (c - 1) (s + b)

-- that was harder to get right than it should've been.
-- here's the obviously-correct version

multiply' :: (Integral a) => a -> a -> a
multiply' a b = sum (genericReplicate a b)
