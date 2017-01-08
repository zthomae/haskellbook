module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
  execState (mapM_ addResult list) []

-- the () was missing (0.12.0-screen)
addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main =
  mapM_ putStrLn $ reverse $ fizzBuzzList [1..100]
