{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (isAlpha)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Test.Hspec
import Text.RawString.QQ
-- parsers 0.12.3, trifecta 1.5.2
import Text.Trifecta

headerEx :: ByteString
headerEx = "[blah]"

-- "[blah]" -> Section "blah"
newtype Header = Header String deriving (Eq, Ord, Show)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser Header
parseHeader =
  parseBracketPair (Header <$> some letter)

assignmentEx :: ByteString
assignmentEx = "woot=1"

type Name = String
type Value = String
type Assignments = Map Name Value

parseAssignment :: Parser (Name, Value)
parseAssignment = do
  name <- some letter
  _ <- char '='
  val <- some (noneOf "\n")
  skipEOL -- important!
  return (name, val)

skipEOL :: Parser ()
skipEOL =  skipMany (oneOf "\n")

commentEx :: ByteString
commentEx =
  "; last modified 1 April\
  \ 2001 by John Doe"

commentEx' :: ByteString
commentEx' =
  "; blah\n; woot\n  \n;hah"

skipComments :: Parser ()
skipComments =
  skipMany (do _ <- char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipEOL)

sectionEx :: ByteString
sectionEx =
  "; ignore me\n[states]\nChris=Texas"

sectionEx' :: ByteString
sectionEx' = [r|
; ignore me
[states]
Chris=Texas
|]

sectionEx'' :: ByteString
sectionEx'' = [r|
; comment
[section]
host=wikipedia.org
alias=claw

[whatisit]
red=intoothandclaw
|]

data Section =
  Section Header Assignments
  deriving (Eq, Show)

newtype Config =
  Config (Map Header Assignments)
  deriving (Eq, Show)

skipWhitespace :: Parser ()
skipWhitespace =
  skipMany (char ' ' <|> char '\n')

parseSection :: Parser Section
parseSection = do
  skipWhitespace
  skipComments
  h <- parseHeader
  skipEOL
  assignments <- some parseAssignment
  return $ Section h (M.fromList assignments)

rollup :: Section -> Map Header Assignments -> Map Header Assignments
rollup (Section h a) m =
  M.insert h a m

parseIni :: Parser Config
parseIni = do
  sections <- some parseSection
  let mapOfSections =
        foldr rollup M.empty sections
  return (Config mapOfSections)

runTest :: (Eq a, Show a) => Parser a -> ByteString -> Maybe a -> Expectation
runTest parser input output =
  case (parseByteString parser mempty input, output) of
    (Success a, Just value) -> a `shouldBe` value
    (Success a, Nothing) -> expectationFailure $ "Parsing " ++ (show input) ++ " should have failed, instead produced " ++ show a
    (Failure error, Just _) -> expectationFailure $ "Parse should have succeeded: " ++ show error
    (Failure error, Nothing) -> return ()

main :: IO ()
main = hspec $ do

  describe "Assignment Parsing" $ do
    it "can parse a simple assignment" $ do
      runTest parseAssignment assignmentEx $ Just ("woot", "1")

  describe "Header Parsing" $ do
    it "can parse a simple header" $ do
      runTest parseHeader headerEx $ Just (Header "blah")

  describe "Comment Parsing" $ do
    it "can skip a comment before a header" $ do
      let p = skipComments >> parseHeader
          i = "; woot\n[blah]"
      runTest (skipComments >> parseHeader) "; woot\n[blah]" $ Just (Header "blah")

  describe "Section Parsing" $ do
    it "can parse a simple section" $ do
      runTest parseSection sectionEx $ Just (Section (Header "states") (M.fromList [("Chris", "Texas")]))

  describe "INI Parsing" $ do
    it "can parse multiple sections" $ do
    let sectionValues = M.fromList
                        [ ("alias", "claw")
                        , ("host", "wikipedia.org")]
        whatisitValues = M.fromList [("red", "intoothandclaw")]
        expected = Just (Config
                         (M.fromList
                          [ (Header "section", sectionValues)
                          , (Header "whatisit", whatisitValues)]))
    runTest parseIni sectionEx'' expected
