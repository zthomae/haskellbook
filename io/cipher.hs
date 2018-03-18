module Main where

import Data.Char (chr, ord)
import System.Environment (getArgs, withArgs)
import System.Exit (die)
import System.IO (hWaitForInput, stdin)

toChar :: Int -> Char
toChar o = chr (ord 'a' + o)

fromChar :: Char -> Int
fromChar c = ord c - ord 'a'

vigenereEncode :: String -> String -> String
vigenereEncode key plaintext = map (toChar . encode) (zip plaintext (cycle key))
  where encode (b, c) =
          let
            b' = fromChar b
            c' = fromChar c
          in
            (b' + c') `mod` 26

vigenereDecode :: String -> String -> String
vigenereDecode key ciphertext = map (toChar . decode) (zip ciphertext (cycle key))
  where decode (b, c) =
          let
            b' = fromChar b
            c' = fromChar c
          in
            (b' - c') `mod` 26

data Mode = Encode | Decode deriving (Eq, Show)
data Args = Args Mode String (Maybe Int) deriving (Eq, Show)

-- stolen from https://alvinalexander.com/source-code/haskell/safe-string-to-int-conversion-in-haskell-example
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

parseArgs :: [String] -> Maybe Args
parseArgs = inner Nothing Nothing Nothing
  where inner _ key timeout ("-e":rest) = inner (Just Encode) key timeout rest
        inner _ key timeout ("-d":rest) = inner (Just Decode) key timeout rest
        inner mode key _ ("-t":timeout:rest) =
          case (readMaybe timeout :: Maybe Int) of
            Just timeout -> inner mode key (Just timeout) rest
            Nothing -> Nothing
        inner (Just mode) _ timeout [key] = Just $ Args mode key timeout
        inner _ _ _ _ = Nothing

printUsage :: IO ()
printUsage = putStrLn "Usage: [command] [-e | -d] [-t <int>] [key]"

encode :: String -> IO String
encode key = do
  plaintext <- getContents
  seq (length plaintext) (return ())
  return $ vigenereEncode key plaintext

decode :: String -> IO String
decode key = do
  ciphertext <- getContents
  seq (length ciphertext) (return ())
  return $ vigenereDecode key ciphertext

runWithArgs :: Args -> IO ()
runWithArgs (Args mode key timeout) = do
  let process =
        case mode of
          Encode -> encode
          Decode -> decode
  case timeout of
    Just timeout -> do
      hasInput <- hWaitForInput stdin timeout
      if hasInput then putStr =<< process key else die "Did not receive input within timeout"
    Nothing -> putStr =<< process key

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just args -> runWithArgs args
    Nothing -> printUsage

testMain :: [String] -> IO ()
testMain args = withArgs args main
