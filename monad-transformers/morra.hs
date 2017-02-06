module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (toLower)

data Score = Score Integer Integer deriving (Eq, Show)

data Player = Odd | Even deriving (Eq, Show)

data Move = One | Two deriving (Eq, Show)

move :: Maybe (Move, Move) -> Score -> (Maybe Player, Score)
move Nothing s = (Nothing, s)
move (Just (One, One)) (Score s1 s2) = (Just Even, Score s1 (s2 + 1))
move (Just (Two, Two)) (Score s1 s2) = (Just Even, Score s1 (s2 + 1))
move (Just _) (Score s1 s2) = (Just Odd, Score (s1 + 1) s2)

loop :: StateT Score IO ()
loop = do
  (winner, score) <- advance
  lift $ maybe (putStrLn "There was no winner") (\a -> putStrLn $ "Winner: " ++ (show a)) winner
  lift $ putStrLn ("Score: " ++ (showScore score))
  continue

continue :: StateT Score IO ()
continue = do
  lift $ putStrLn "Do you wish to continue? (yes/no)"
  response <- lift getLine
  case (toLower <$> response) of
    "yes" -> loop
    "no" -> return ()
    _ -> do
      lift $ putStrLn "Invalid response"
      continue

advance :: StateT Score IO (Maybe Player, Score)
advance = do
  s <- get
  (winner, score) <- lift $ step s
  put score
  return (winner, score)

step :: Score -> IO (Maybe Player, Score)
step s = do
  maybeStep <- getMove
  return $ move maybeStep s

moveFromString :: String -> Maybe Move
moveFromString "one" = Just One
moveFromString "two" = Just Two
moveFromString _ = Nothing

getMove :: IO (Maybe (Move, Move))
getMove = do
  putStr "First move: "
  input1 <- getLine
  putStr "Second move: "
  input2 <- getLine
  case (moveFromString input1, moveFromString input2) of
    (Just m1, Just m2) -> return $ Just (m1, m2)
    _ -> do
      putStrLn "Invalid move"
      return $ Nothing

showScore :: Score -> String
showScore (Score a b) = concat ["Odd: ", (show a), ", Even: ", (show b)]

main :: IO ()
main = do
  let initialState = Score 0 0
  (evalStateT loop) initialState
