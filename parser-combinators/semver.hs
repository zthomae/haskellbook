module SemVerParsing where

import Control.Applicative
import Data.Ord
import Test.Hspec
import Test.QuickCheck hiding (Result, Success)
import Text.Trifecta

data NumberOrString =
  NOSS String
  | NOSI Integer
  deriving (Eq, Show)

instance Ord NumberOrString where
  (NOSS a) <= (NOSS b) = a <= b
  (NOSI a) <= (NOSI b) = a <= b
  (NOSI _) <= (NOSS _) = True
  (NOSS _) <= (NOSI _) = False

instance Arbitrary NumberOrString where
  arbitrary = do
    s <- arbitrary
    i <- arbitrary
    oneof [ return (NOSS s)
          , return (NOSI i)
          ]

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer =
  SemVer Major Minor Patch Release Metadata deriving (Eq, Show)

instance Ord SemVer where
  compare (SemVer maj1 min1 pat1 rel1 _) (SemVer maj2 min2 pat2 rel2 _) =
    case (compare maj1 maj2, compare min1 min2, compare pat1 pat2, compare rel1 rel2) of
      (LT, _, _, _) -> LT
      (EQ, LT, _, _) -> LT
      (EQ, EQ, LT, _) -> LT
      (EQ, EQ, EQ, LT) -> LT
      (EQ, EQ, EQ, EQ) -> EQ
      _ -> GT

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = NOSI <$> try ((integer <* notFollowedBy (alphaNum <|> char '-')))
                      <|> NOSS <$> (some (alphaNum <|> char '-'))

parseSemVer :: Parser SemVer
parseSemVer = do
  major <- integer
  _ <- char '.'
  minor <- integer
  _ <- char '.'
  patch <- integer
  release <- option [] (char '-' *> sepBy1 parseNumberOrString (char '.'))
  metadata <- option [] (char '+' *> sepBy1 parseNumberOrString (char '.'))
  return $ SemVer major minor patch release metadata

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

main :: IO ()
main = hspec $ do
  describe "SemVer Parsing" $ do
    it "should parse a simple version" $ do
      let m = parseString parseSemVer mempty "2.1.1"
          r = maybeSuccess m
      r `shouldBe` (Just $ SemVer 2 1 1 [] [])

    it "should parse a version with a pre-release section" $ do
      let m = parseString parseSemVer mempty "1.0.0-x.7.z.92"
          r = maybeSuccess m
      r `shouldBe` (Just $ SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])

    it "should parse a version with a metadata section" $ do
      let m = parseString parseSemVer mempty "1.0.0+20130313144700"
          r = maybeSuccess m
      r `shouldBe` (Just $ SemVer 1 0 0 [] [NOSI 20130313144700])

    it "should parse a version with a pre-release section and a metadata section" $ do
      let m = parseString parseSemVer mempty "1.0.0-beta+exp.sha.5114f85"
          r = maybeSuccess m
      r `shouldBe` (Just $ SemVer 1 0 0 [NOSS "beta"] [NOSS "exp", NOSS "sha", NOSS "5114f85"])

  describe "NumberOrString Ord" $ do
    it "should compare strings lexicographically" $ do
      property $ \ a b -> compare a b == compare (NOSS a) (NOSS b)

    it "should compare numbers numerically" $ do
      property $ \ a b -> compare a b == compare (NOSI a) (NOSI b)

    it "should always find that numbers are less than strings" $ do
      property $ \ i s -> compare (NOSI i) (NOSS s) == LT

  describe "SemVer Ord" $ do
    it "should compare major versions" $ do
      property $ \ a b w x y z -> compare a b == compare (SemVer a w x y z) (SemVer b w x y z)

    it "should compare minor versions" $ do
      property $ \ m1 m2 a b x y z ->
        compare (SemVer m1 a x y z) (SemVer m2 b x y z) ==
          case (compare m1 m2) of
            LT -> LT
            EQ -> compare a b
            _ -> GT

    it "should compare patch versions" $ do
      property $ \ a b maj1 maj2 min1 min2 x y ->
        compare (SemVer maj1 min1 a x y) (SemVer maj2 min2 b x y) ==
          case (compare maj1 maj2, compare min1 min2) of
            (LT, _) -> LT
            (EQ, LT) -> LT
            (EQ, EQ) -> compare a b
            _ -> GT

    it "should compare string pre-releases" $ do
      property $ \ a b maj1 maj2 min1 min2 patch1 patch2 x ->
        compare (SemVer maj1 min1 patch1 a x) (SemVer maj2 min2 patch2 b x) ==
          case (compare maj1 maj2, compare min1 min2, compare patch1 patch2) of
            (LT, _, _) -> LT
            (EQ, LT, _) -> LT
            (EQ, EQ, LT) -> LT
            (EQ, EQ, EQ) -> compare a b
            _ -> GT

    it "should not compare metadata" $ do
      property $ \ a b w x y z -> compare (SemVer w x y z a) (SemVer w x y z b) == EQ
