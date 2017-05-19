module PhoneNumberParsing where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (foldl')
import Data.Ord
import Test.Hspec
import Test.QuickCheck hiding (Result, Success)
import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

sumDigits :: [Char] -> Int
sumDigits = fromIntegral . (foldl' (\ n c -> (n * 10) + (toInteger (digitToInt c))) 0)

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

-- format 1: 123-456-7890
parseFormat1 = do
  area <- count 3 parseDigit
  _ <- char '-'
  exchange <- count 3 parseDigit
  _ <- char '-'
  line <- count 4 parseDigit
  return $ PhoneNumber (sumDigits area) (sumDigits exchange) (sumDigits line)

-- format 2: 1234567890
parseFormat2 = do
  area <- count 3 parseDigit
  exchange <- count 3 parseDigit
  line <- count 4 parseDigit
  return $ PhoneNumber (sumDigits area) (sumDigits exchange) (sumDigits line)

-- format 3: (123) 456-7890
parseFormat3 = do
  area <- between (char '(') (char ')') (count 3 parseDigit)
  _ <- char ' '
  exchange <- count 3 parseDigit
  _ <- char '-'
  line <- count 4 parseDigit
  return $ PhoneNumber (sumDigits area) (sumDigits exchange) (sumDigits line)

-- format 4: 1-123-456-7890
parseFormat4 = do
  _ <- string "1-"
  area <- count 3 parseDigit
  _ <- char '-'
  exchange <- count 3 parseDigit
  _ <- char '-'
  line <- count 4 parseDigit
  return $ PhoneNumber (sumDigits area) (sumDigits exchange) (sumDigits line)

parsePhone :: Parser PhoneNumber
parsePhone = choice [try parseFormat1, try parseFormat2, try parseFormat3, try parseFormat4]

main :: IO ()
main = hspec $ do
  let expected = Just $ PhoneNumber 123 456 7890
  describe "parsePhone" $ do
    it "should parse format 1" $ do
      let m = parseString parsePhone mempty "123-456-7890"
          r = maybeSuccess m
      r `shouldBe` expected
    it "should parse format 2" $ do
      let m = parseString parsePhone mempty "1234567890"
          r = maybeSuccess m
      r `shouldBe` expected
    it "should parse format 3" $ do
      let m = parseString parsePhone mempty "(123) 456-7890"
          r = maybeSuccess m
      r `shouldBe` expected
    it "should parse format 4" $ do
      let m = parseString parsePhone mempty "1-123-456-7890"
          r = maybeSuccess m
      r `shouldBe` expected
