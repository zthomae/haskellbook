module Main where

import Criterion.Main
import Data.Maybe (fromJust)

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Eq, Show)

-- adds an item
push :: a -> Queue a -> Queue a
push x (Queue [] []) = Queue [] [x]
push x (Queue xs ys) = Queue (x:xs) ys

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue ys [x]) = Just (x, Queue [] $ reverse ys)
pop (Queue ys (x:xs)) = Just (x, Queue ys xs)
pop _ = Nothing

toList :: Queue a -> [a]
toList (Queue back front) = front ++ reverse back

empty :: Queue a
empty = Queue [] []

shouldPush1 :: Int -> Bool
shouldPush1 x
  | x `mod` 12 > 8 = False
  | otherwise = True

shouldPush2 :: Int -> Bool
shouldPush2 x
  | x `mod` 12 > 8 = False
  | x `mod` 12 > 5 = True
  | x `mod` 12 > 2 = False
  | otherwise = True

shouldPush3 :: Int -> Bool
shouldPush3 x
  | x `mod` 12 > 8 = False
  | x `mod` 12 > 5 = False
  | x `mod` 12 > 2 = True
  | otherwise = True

shouldPush4 :: Int -> Bool
shouldPush4 x
  | x `mod` 12 > 8 = False
  | x `mod` 12 > 5 = True
  | x `mod` 12 > 2 = True
  | otherwise = True

-- my tests are using partial functions like init and fromJust. I've tried to be careful.
listBench :: (Int -> Bool) -> Int -> [Int]
listBench shouldPush i = reverse $ go 0 []
  where go n acc
          | n == i = acc
          | otherwise = go (n+1) (if shouldPush n then n:acc else init acc)

queueBench :: (Int -> Bool) -> Int -> [Int]
queueBench shouldPush i = toList $ go 0 empty
  where go n acc
          | n == i = acc
          | otherwise = go (n+1) (if shouldPush n then push n acc else snd (fromJust (pop acc)))

main :: IO ()
main = defaultMain
  [ bench "list queue (scheme 1)" $
    whnf (listBench shouldPush1) 100
  , bench "our queue (scheme 1)" $
    whnf (queueBench shouldPush1) 100
  , bench "list queue (scheme 2)" $
    whnf (listBench shouldPush2) 100
  , bench "our queue (scheme 2)" $
    whnf (queueBench shouldPush2) 100
  , bench "list queue (scheme 3)" $
    whnf (listBench shouldPush3) 100
  , bench "our queue (scheme 3)" $
    whnf (queueBench shouldPush3) 100
  , bench "list queue (scheme 4)" $
    whnf (listBench shouldPush4) 100
  , bench "our queue (scheme 4)" $
    whnf (queueBench shouldPush4) 100
  ]
