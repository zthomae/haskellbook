module Ipv4Parser where

import Data.Bits ((.&.))
import Data.Char (digitToInt)
import Data.List (foldl', intercalate)
import Data.Maybe (catMaybes)
import Data.Ord
import Data.Word
import Test.Hspec
import Test.QuickCheck hiding ((.&.), Failure, Result, Success)
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta

newtype IPAddress = IPAddress Word32 deriving (Eq, Ord)

instance Show IPAddress where
  show (IPAddress ip) =
    let
      one = (ip .&. 0xff000000) `div` 2^24
      two = (ip .&. 0x00ff0000) `div` 2^16
      three = (ip .&. 0x0000ff00) `div` 2^8
      four = ip .&. 0x000000ff
    in
      intercalate "." [show one, show two, show three, show four]

parseOctet :: Parser Word8
parseOctet = do
  first <- digit
  rest <- many digit
  let sum = foldl' (\b a -> b * 10 + a) 0 (digitToInt <$> (first:rest))
  if sum > 255
    then fail "octet must be between 0 and 255"
    else return $ fromIntegral sum

parseIpv4 :: Parser IPAddress
parseIpv4 = do
  one <- parseOctet
  char '.'
  two <- parseOctet
  char '.'
  three <- parseOctet
  char '.'
  four <- parseOctet
  return (IPAddress $ fromIntegral four + (fromIntegral three * (2^8) + (fromIntegral two * (2^16) + (fromIntegral one * (2^24)))))

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

runTest :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Expectation
runTest parser input output =
  case (parseString parser mempty input, output) of
    (Success a, Just value) -> a `shouldBe` value
    (Success a, Nothing) -> expectationFailure $ "Parsing " ++ input ++ " should have failed, instead produced " ++ show a
    (Failure error, Just _) -> expectationFailure $ "Parse should have succeeded: " ++ show error
    (Failure error, Nothing) -> return ()

main :: IO ()
main = hspec $ do
  describe "parseIPv4" $ do
    let test = runTest parseIpv4

    it "should parse example 1" $ test "172.16.254.1" $ Just (IPAddress 2886794753)
    it "should parse example 2" $ test "204.120.0.15" $ Just (IPAddress 3430416399)

    it "should round trip all Word32s" $ property $ do
      w <- (arbitrary :: Gen Word32)
      let ip = IPAddress w
      return $ test (show ip) (Just ip)
