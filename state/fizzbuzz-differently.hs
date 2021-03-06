module FizzBuzzDifferently where

import Control.Monad
import Control.Monad.Trans.State
import Data.Functor.Identity

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = runIdentity $ execStateT (mapM_ addResult [to, to - 1 .. from]) []
