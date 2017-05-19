module IntegerParsing where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Ord
import Test.Hspec
import Test.QuickCheck hiding (Result, Success)
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

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "parseDigit" $ do
    it "should parse a single digit" $ do
      let m = parseString parseDigit mempty "123"
          r = maybeSuccess m
      r `shouldBe` (Just '1')
    it "should not parse non-digits" $ do
      let m = parseString parseDigit mempty "a23"
          r = maybeSuccess m
      r `shouldBe` Nothing

  describe "base10Integer" $ do
    it "should parse strings that are entirely digits" $ do
      let m = parseString base10Integer mempty "123"
          r = maybeSuccess m
      r `shouldBe` (Just 123)

    it "should consume a string that is partially a positive integer" $ do
      let m = parseString base10Integer mempty "321abc"
          r = maybeSuccess m
      r `shouldBe` (Just 321)

    it "should parse very large numbers" $ do
      let m = parseString base10Integer mempty "999999999999999999999999999999999999999999"
          r = maybeSuccess m
      r `shouldBe` (Just 999999999999999999999999999999999999999999)

    it "should not parse negative numbers" $ do
      let m = parseString base10Integer mempty "-1"
          r = maybeSuccess m
      r `shouldBe` Nothing

  describe "positiveOrNegativeInteger" $ do
    it "should parse positive numbers" $ do
      let m = parseString positiveOrNegativeInteger mempty "123"
          r = maybeSuccess m
      r `shouldBe` (Just 123)

    it "should parse negative numbers" $ do
      let m = parseString positiveOrNegativeInteger mempty "-123"
          r = maybeSuccess m
      r `shouldBe` (Just (-123))

    it "should only allow a single hyphen" $ do
      let m = parseString positiveOrNegativeInteger mempty "--123"
          r = maybeSuccess m
      r `shouldBe` Nothing
