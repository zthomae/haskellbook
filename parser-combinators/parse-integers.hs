module IntegerParsing where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Ord
import Test.Hspec
import Test.QuickCheck hiding (Failure, Result, Success)
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

sumDigits :: [Char] -> Integer
sumDigits = foldl' (\ n c -> (n * 10) + (toInteger (digitToInt c))) 0

base10Integer :: Parser Integer
base10Integer = do
  digits <- some parseDigit
  return $ sumDigits digits

positiveOrNegativeInteger :: Parser Integer
positiveOrNegativeInteger = do
  scale <- option 1 (try (char '-' *> pure (-1)))
  number <- base10Integer
  return $ scale * number

runTest :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Expectation
runTest parser input output =
  case (parseString parser mempty input, output) of
    (Success a, Just value) -> a `shouldBe` value
    (Success a, Nothing) -> expectationFailure $ "Parsing " ++ input ++ " should have failed, instead produced " ++ show a
    (Failure error, Just _) -> expectationFailure $ "Parse should have succeeded: " ++ show error
    (Failure error, Nothing) -> return ()

main :: IO ()
main = hspec $ do
  describe "parseDigit" $ do
    let test = runTest parseDigit
    it "should parse a single digit" $ do
      test "123" (Just '1')

    it "should not parse non-digits" $ do
      test "a23" Nothing

  describe "base10Integer" $ do
    let test = runTest base10Integer
    it "should parse strings that are entirely digits" $ do
      test "123" (Just 123)

    it "should consume a string that is partially a positive integer" $ do
      test "321abc" (Just 321)

    it "should parse very large numbers" $ do
      test "999999999999999999999999999999999999999999" (Just 999999999999999999999999999999999999999999)

    it "should not parse negative numbers" $ do
      test "-1" Nothing

  describe "positiveOrNegativeInteger" $ do
    let test = runTest positiveOrNegativeInteger
    it "should parse positive numbers" $ do
      test "123" (Just 123)

    it "should parse negative numbers" $ do
      test "-123" (Just (-123))

    it "should only allow a single hyphen" $ do
      test "--123" Nothing
