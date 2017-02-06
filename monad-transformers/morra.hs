module Morra where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Char (toLower)
import System.Random (randomIO)

data Score = Score Integer Integer deriving (Eq, Show)

data Player = Odd | Even deriving (Eq, Show)

data Move = One | Two deriving (Eq, Show)

type Step = IO (Maybe (Move, Move))

data GameState = GameState Step Score

move :: Maybe (Move, Move) -> Score -> (Maybe Player, Score)
move Nothing s = (Nothing, s)
move (Just (One, One)) (Score s1 s2) = (Just Even, Score s1 (s2 + 1))
move (Just (Two, Two)) (Score s1 s2) = (Just Even, Score s1 (s2 + 1))
move (Just _) (Score s1 s2) = (Just Odd, Score (s1 + 1) s2)

loop :: StateT GameState IO ()
loop = do
  (winner, score) <- advance
  lift $ maybe (putStrLn "There was no winner") (\a -> putStrLn $ "Winner: " ++ (show a)) winner
  lift $ putStrLn ("Score: " ++ (showScore score))
  continue

continue :: StateT GameState IO ()
continue = do
  lift $ putStrLn "Do you wish to continue? (yes/no)"
  response <- lift getLine
  case (toLower <$> response) of
    "yes" -> loop
    "no" -> return ()
    _ -> do
      lift $ putStrLn "Invalid response"
      continue

advance :: StateT GameState IO (Maybe Player, Score)
advance = do
  (GameState step s) <- get
  moves <- lift step
  case moves of
    Nothing -> return (Nothing, s)
    _ -> do
      let (winner, score) = move moves s
      put $ GameState step score
      return (winner, score)

moveFromString :: String -> Maybe Move
moveFromString "one" = Just One
moveFromString "two" = Just Two
moveFromString _ = Nothing

getMoveFromPlayer :: IO (Maybe Move)
getMoveFromPlayer = do
  move <- getLine
  case (moveFromString move) of
    Nothing -> do
      putStrLn "Invalid move"
      return Nothing
    m -> return m

getMoveRandomly :: IO Move
getMoveRandomly = do
  choice <- (randomIO :: IO Integer)
  let r = choice `rem` 2
  return $ if r == 0 then One else Two

randomMoveStep :: Step
randomMoveStep = do
  putStr "Player 1 move: "
  playerMove <- getMoveFromPlayer
  case playerMove of
    Just m -> do
      randomMove <- getMoveRandomly
      return $ Just (m, randomMove)
    _ -> return Nothing

humanMoveStep :: Step
humanMoveStep = do
  putStr "Player 1 move: "
  player1Move <- getMoveFromPlayer
  putStr "Player 2 move: "
  player2Move <- getMoveFromPlayer
  case (player1Move, player2Move) of
    (Just m1, Just m2) -> return $ Just (m1, m2)
    _ -> return Nothing

showScore :: Score -> String
showScore (Score a b) = concat ["Odd: ", (show a), ", Even: ", (show b)]

chooseMode = do
  putStr "Choose a mode (random or human): "
  modeResponse <- getLine
  case (toLower <$> modeResponse) of
          "random" -> return randomMoveStep
          "human" -> return humanMoveStep
          _ -> do
            putStrLn "Invalid mode response"
            chooseMode

main :: IO ()
main = do
  mode <- chooseMode
  let initialState = GameState mode (Score 0 0)
  (evalStateT loop) initialState
