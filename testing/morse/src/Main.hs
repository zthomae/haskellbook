module Main where

import Control.Monad (forever, when)
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)

-- This code is tweaked from the book. I think it could use even more work.

mainConvert :: (String -> IO a) -> IO b
mainConvert convertLine = forever $ do
  weAreDone <- hIsEOF stdin
  when weAreDone exitSuccess
  line <- hGetLine stdin
  convertLine line

convertToMorse :: IO ()
convertToMorse = mainConvert convertLine
  where
    convertLine line = do
      let morse = stringToMorse line
      case morse of
        (Just str) -> putStrLn $ intercalate " " str
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

convertFromMorse :: IO ()
convertFromMorse = mainConvert convertLine
  where
    convertLine line = do
      let decoded :: Maybe String
          decoded = traverse morseToChar (words line)
      case decoded of
        (Just s) -> putStrLn s
        Nothing -> do
          putStrLn $ "ERROR: " ++ line
          exitFailure

main :: IO ()
main = do
  mode <- getArgs
  case mode of
    [arg] ->
      case arg of
        "from" -> convertFromMorse
        "to" -> convertToMorse
        _ -> argError
    _ -> argError
  where argError = do
          putStrLn "Please specify the first argument as being 'from' or 'to' morse, such as: morse to"
          exitFailure
