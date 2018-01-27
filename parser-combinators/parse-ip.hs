module IpParser where

import Data.Bits ((.&.))
import Data.Char (chr, digitToInt, ord)
import Data.List (foldl', intercalate, partition)
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

octet :: Parser Word8
octet = do
  first <- digit
  rest <- many digit
  if (length (first:rest) > 3)
    then fail "octet can only be three characters long"
    else do
      let sum = foldl' (\b a -> b * 10 + a) 0 (digitToInt <$> (first:rest))
      if sum > 255
        then fail "octet must be between 0 and 255"
        else return $ fromIntegral sum

ipv4 :: Parser IPAddress
ipv4 = do
  one <- octet
  char '.'
  two <- octet
  char '.'
  three <- octet
  char '.'
  four <- octet
  return (IPAddress $ fromIntegral four + (fromIntegral three * (2^8) + (fromIntegral two * (2^16) + (fromIntegral one * (2^24)))))

data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

-- TODO: Write my own Show instance
newtype Hextet = Hextet Word16 deriving (Eq, Ord, Show)

-- this is not very good
hexToInt :: Char -> Maybe Int
hexToInt c
  | c >= '0' && c <= '9' = Just $ ord c - ord '0'
  | c >= 'a' && c <= 'f' = Just $ ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' = Just $ ord c - ord 'A' + 10
  | otherwise = Nothing

hex :: Parser Int
hex = do
  c <- alphaNum
  case hexToInt c of
    Just n -> return n
    Nothing -> fail $ "not a valid hex digit: " ++ [c]

hextet :: Parser Hextet
hextet = do
  first <- hex
  rest <- many hex
  if length (first:rest) > 4
    then fail "octet cannot be more than 4 characters long"
    else do
      let sum = foldl' (\b a -> b * 16 + a) 0 (first:rest)
      if sum >= 2^16
        then fail "a hextet must be between 0 and (2^16)-1"
        else return (Hextet $ fromIntegral sum)

hextets :: Parser [Hextet]
hextets = sepBy1 hextet (try $ char ':' <* notFollowedBy (char ':'))

data Piece = Hextets [Hextet] | Gap deriving (Eq, Ord, Show)

gap :: Parser Piece
gap = do
  string "::"
  return Gap

pieces :: Parser [Piece]
pieces = some (choice [Hextets <$> hextets, gap])

normalizeHextets :: ([Hextet], [Hextet]) -> ([Hextet], [Hextet])
normalizeHextets (before, after)
  | length before > 4 = ((take 4 before), (join (drop 4 before) after))
  | length after > 4 = ((join before (take (length after - 4) after)), (drop (length after - 4) after))
  | otherwise = (before ++ replicate (4 - length before) (Hextet 0), replicate (4 - length after) (Hextet 0) ++  after)
    where join xs ys = xs ++ (replicate (4 - length xs - length ys) (Hextet 0)) ++ ys
          takeRight n xs = drop (length xs - n) xs
          dropRight n xs = take (length xs - n) xs

toHextets :: [Piece] -> [Hextet]
toHextets ps = foldl' combine [] ps
  where combine acc Gap = acc
        combine acc (Hextets hs) = acc ++ hs

toHextets2 :: [Piece] -> ([Hextet], [Hextet])
toHextets2 (Gap:(Hextets hs:[])) = ([], hs)
toHextets2 (Hextets hs:[]) = (hs, [])
toHextets2 (Hextets hs:(Gap:[])) = (hs, [])
toHextets2 (Hextets hs:(Gap:(Hextets hs':[]))) = (hs, hs')

sumHextets :: [Hextet] -> Word64
sumHextets = foldl' (\acc (Hextet next) -> (acc * 2^16) + (fromIntegral next)) (fromIntegral 0)

-- TODO: conditions should be monadic
ipv6 :: Parser IPAddress6
ipv6 = do
  ps <- pieces
  let (gs, hs) = partition ((==) Gap) ps
  let gapNeeded = 8 - length (toHextets ps)
  if length gs > 1
    then fail "An IPv6 address can only be abbreviated with :: once"
    else if (length gs == 0 && gapNeeded > 0) || (gapNeeded < 0)
      then fail "An IPv6 address must have 8 hextets"
      else let (first, rest) = normalizeHextets $ toHextets2 ps
             in return $ IPAddress6 (sumHextets first) (sumHextets rest)

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
    let test = runTest ipv4

    it "should parse example 1" $ test "172.16.254.1" $ Just (IPAddress 2886794753)
    it "should parse example 2" $ test "204.120.0.15" $ Just (IPAddress 3430416399)

    it "should round trip all Word32s" $ property $ do
      w <- (arbitrary :: Gen Word32)
      let ip = IPAddress w
      return $ test (show ip) (Just ip)

  describe "hex" $ do
    let test = runTest hex

    it "should parse example 1" $ test "A" $ Just 10
    it "should parse example 2" $ test "0" $ Just 0
    it "should parse example 3" $ test "a" $ Just 10

    it "should not parse example 4" $ test "g" $ Nothing

  describe "hextet" $ do
    let test = runTest hextet

    it "should parse 0000" $ test "0000" $ Just (Hextet 0)
    it "should parse 0" $ test "0" $ Just (Hextet 0)
    it "should parse 000" $ test "000" $ Just (Hextet 0)
    it "should parse FF" $ test "FF" $ Just (Hextet 255)
    it "should parse f" $ test "f" $ Just (Hextet 15)

    it "should not parse fffff" $ test "fffff" $ Nothing
    it "should not parse 0000a" $ test "0000a" $ Nothing

  describe "hextets" $ do
    let test = runTest hextets

    it "should parse 0" $ test "0" $ Just $ [Hextet 0]
    it "should parse 0:0" $ test "0:0" $ Just $ [Hextet 0, Hextet 0]
    it "should parse 0:ffff" $ test "0:ffff" $ Just $ [Hextet 0, Hextet 65535]
    it "should parse 0:0:0:0:0:ffff:ac10:fe01" $ test "0:0:0:0:0:ffff:ac10:fe01" $ Just [Hextet 0, Hextet 0, Hextet 0, Hextet 0, Hextet 0, Hextet 65535, Hextet 44048, Hextet 65025]

    it "should not parse 0:g" $ test "0:g" $ Nothing

  describe "gap" $ do
    let test = runTest gap

    it "should parse ::" $ test "::" $ Just Gap

  describe "pieces" $ do
    let test = runTest pieces

    it "should parse 0::0" $ test "0::0" $ Just [Hextets [Hextet 0], Gap, Hextets [Hextet 0]]
    it "should parse ::" $ test "::" $ Just [Gap]
    it "should parse 0:0:0:0::" $ test "0:0:0:0::" $ Just [Hextets [Hextet 0, Hextet 0, Hextet 0, Hextet 0], Gap]

  describe "normalizeHextets" $ do
    it "should expand the first hextet" $
      normalizeHextets ([Hextet 0, Hextet 0, Hextet 0], [Hextet 0, Hextet 0, Hextet 0])
      `shouldBe`
      ([Hextet 0, Hextet 0, Hextet 0, Hextet 0], [Hextet 0, Hextet 0, Hextet 0, Hextet 0])
    it "should expand the second hextet" $
      normalizeHextets ([Hextet 0, Hextet 0, Hextet 0, Hextet 0], [Hextet 0, Hextet 0, Hextet 0])
      `shouldBe`
      ([Hextet 0, Hextet 0, Hextet 0, Hextet 0], [Hextet 0, Hextet 0, Hextet 0, Hextet 0])
    it "should move hextets from the first list to the second" $
      normalizeHextets ([Hextet 1, Hextet 2, Hextet 3, Hextet 4, Hextet 5], [Hextet 6, Hextet 7])
      `shouldBe`
      ([Hextet 1, Hextet 2, Hextet 3, Hextet 4], [Hextet 5, Hextet 0, Hextet 6, Hextet 7])
    it "should move hextets from the second list to the first" $
      normalizeHextets ([Hextet 1, Hextet 2], [Hextet 3, Hextet 4, Hextet 5, Hextet 6, Hextet 7])
      `shouldBe`
      ([Hextet 1, Hextet 2, Hextet 0, Hextet 3], [Hextet 4, Hextet 5, Hextet 6, Hextet 7])
    it "should normalize ffff::" $
      normalizeHextets ([Hextet 65535], [Hextet 0, Hextet 0, Hextet 0, Hextet 0])
      `shouldBe`
      ([Hextet 65535, Hextet 0, Hextet 0, Hextet 0], [Hextet 0, Hextet 0, Hextet 0, Hextet 0])

  describe "ipv6" $ do
    let test = runTest ipv6

    it "should not parse ::::" $ test "::::" $ Nothing
    it "should not parse 0:0::0:0::0" $ test "0:0::0:0::0" $ Nothing
    it "should not parse 0:0:0:0:0:0:0" $ test "0:0:0:0:0:0:0" $ Nothing
    it "should not parse 0:0:0:0:0:0:0:0:0" $ test "0:0:0:0:0:0:0:0:0" $ Nothing

    it "should parse 0:0:0:0:0:ffff:ac10:fe01" $ test "0:0:0:0:0:ffff:ac10:fe01" $ Just (IPAddress6 0 281473568538113)
    it "should parse 0:0:0:0:0:ffff:cc78:f" $ test "0:0:0:0:0:ffff:cc78:f" $ Just (IPAddress6 0 281474112159759)
    it "should parse FE80:0000:0000:0000:0202:B3FF:FE1E:8329" $ test "FE80:0000:0000:0000:0202:B3FF:FE1E:8329" $ Just (IPAddress6 18338657682652659712 144876050090722089)
    it "should parse 2001:DB8:0:0:8:800:200C:417A" $ test "2001:DB8:0:0:8:800:200C:417A" $ Just (IPAddress6 2306139568115548160 2260596444381562)
    it "should parse FE80::0202:B3FF:FE1E:8329" $ test "FE80::0202:B3FF:FE1E:8329" $ Just (IPAddress6 18338657682652659712 144876050090722089)
    it "should parse 2001:DB8::8:800:200C:417A" $ test "2001:DB8::8:800:200C:417A" $ Just (IPAddress6 2306139568115548160 2260596444381562)
