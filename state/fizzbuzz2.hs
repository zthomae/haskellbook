module FizzBuzz2 where

import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Buzz"
           | n `mod` 3 == 0 = "Fizz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> DL.DList String
fizzBuzzList list =
  execState (mapM_ addResult list) DL.empty

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main :: IO ()
main =
  mapM_ putStrLn $ fizzBuzzList [1..100]
