{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Data.Ini where

import Prelude hiding (readFile)

import Control.Applicative
import Data.ByteString (ByteString, readFile)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Traversable (traverse)
import System.Directory (getCurrentDirectory, listDirectory)
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

isIniFile :: FilePath -> Bool
isIniFile fp =
  case reverse fp of
    'i':'n':'i':'.':_ -> True
    _ -> False

iniFiles :: FilePath -> IO [FilePath]
iniFiles wd = filter isIniFile <$> listDirectory wd

parseIniFile :: FilePath -> IO (Maybe (FilePath, Config))
parseIniFile fp = do
  contents <- readFile fp
  return $ case parseByteString parseIni mempty contents of
    Success config -> Just (fp, config)
    Failure _ -> Nothing

readInis :: FilePath -> IO (Map FilePath Config)
readInis fp = do
  files <- iniFiles fp
  assocs <- traverse parseIniFile files
  return $ M.fromList (catMaybes assocs)

readInisInCurrentDirectory :: IO (Map FilePath Config)
readInisInCurrentDirectory = getCurrentDirectory >>= readInis
