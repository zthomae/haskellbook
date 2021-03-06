module LogParser where

import Control.Applicative
import Data.Char (digitToInt)
import Data.List (foldl', intercalate)
import qualified Data.Map.Strict as M
import Data.Maybe (catMaybes)
import Data.Ord
import Test.Hspec
import Test.QuickCheck hiding (Failure, Result, Success)
import Text.Parser.LookAhead (lookAhead)
import Text.Trifecta

type Hour = Int
type Minute = Int
data Timestamp = Timestamp Hour Minute deriving (Eq, Ord, Show)
type Activity = String
type Entry = (Timestamp, Activity)

type Year = Int
type Month = Int
type Day = Int
data Date = Date Year Month Day deriving (Eq, Ord, Show)

type Log = M.Map Date [Entry]

sumDigits :: [Char] -> Int
sumDigits = fromIntegral . (foldl' (\ n c -> (n * 10) + (toInteger (digitToInt c))) 0)

eat :: Parser a -> Parser ()
eat = (<*) $ return ()

isFollowedByOrEof :: Parser a -> Parser ()
isFollowedByOrEof parser = do
  lookAhead (eat parser <|> eof)
  return ()

parseComment :: Parser ()
parseComment = do
  many (char ' ')
  string "--"
  some (noneOf "\n")
  return ()

parseCommentOrNewlineOrEof :: Parser ()
parseCommentOrNewlineOrEof = (try parseComment <|> (isFollowedByOrEof $ char '\n'))

parseTimestamp :: Parser Timestamp
parseTimestamp = do
  hour <- count 2 digit
  char ':'
  minute <- count 2 digit
  return $ Timestamp (sumDigits hour) (sumDigits minute)

-- I question whether this is a good idea...
parseActivity :: Parser Activity
parseActivity = manyTill (noneOf "\n") parseCommentOrNewlineOrEof

parseEntry :: Parser (Timestamp, Activity)
parseEntry = do
  ts <- parseTimestamp
  char ' '
  activity <- parseActivity
  return (ts, activity)

parseMaybeEntry :: Parser (Maybe (Timestamp, Activity))
parseMaybeEntry = (Just <$> (try parseEntry)) <|> (parseComment >> return Nothing)

parseDateLine :: Parser Date
parseDateLine = do
  string "# "
  year <- count 4 digit
  char '-'
  month <- count 2 digit
  char '-'
  day <- count 2 digit
  optional (try parseComment)
  isFollowedByOrEof newline
  return $ Date (sumDigits year) (sumDigits month) (sumDigits day)

parseDayList :: Parser (Date, [Entry])
parseDayList = do
  date <- parseDateLine
  newline
  entries <- sepEndBy parseMaybeEntry newline
  return (date, catMaybes entries)

parseMaybeDayList :: Parser (Maybe (Date, [Entry]))
parseMaybeDayList = (Just <$> (try parseDayList)) <|> (parseCommentOrNewlineOrEof >> return Nothing)

parseLog :: Parser Log
parseLog = do
  days <- sepEndBy parseMaybeDayList newline
  return $ M.fromList (catMaybes days)

runTest :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Expectation
runTest parser input output =
  case (parseString parser mempty input, output) of
    (Success a, Just value) -> a `shouldBe` value
    (Success a, Nothing) -> expectationFailure $ "Parsing " ++ input ++ " should have failed, instead produced " ++ show a
    (Failure error, Just _) -> expectationFailure $ "Parse should have succeeded: " ++ show error
    (Failure error, Nothing) -> return ()

main :: IO ()
main = hspec $ do
  describe "parseComment" $ do
    let test = runTest parseComment

    it "should parse a comment at the start of the line" $ do
      test "-- whee a comment" $ Just ()

    it "should parse a comment with whitespace before it" $ do
      test "   -- space!" $ Just ()

    it "should not consume whitespace following it" $ do
      let commentAndNewline = do
            parseComment
            char '\n'
            return ()
      runTest commentAndNewline "-- test\n" $ Just ()

  describe "parseTimestamp" $ do
    it "should parse a valid timestamp" $ do
      runTest parseTimestamp "01:23" $ Just (Timestamp 1 23)

  describe "parseActivity" $ do
    let test = runTest parseActivity

    it "should parse an activity ending in eof" $ do
      test "Sanitizing moisture collector" $ Just "Sanitizing moisture collector"

    it "should parse an activity ending in newline" $ do
      test "Sanitizing moisture collector\n" $ Just "Sanitizing moisture collector"

    it "should parse an activity with a comment after a whitespace" $ do
      test "Breakfast -- should I try skippin bfast?" $ Just "Breakfast"

    it "should parse an activity immediately ending in a comment" $ do
      test "Breakfast--should I try skippin bfast?" $ Just "Breakfast"

    it "should not consume a newline after an activity" $ do
      let activityAndNewline = do
            activity <- parseActivity
            newline
            return activity
      runTest activityAndNewline "Something\n" $ Just "Something"

  describe "parseEntry" $ do
    let test = runTest parseEntry

    it "should parse an entry ending in newline" $ do
      test "22:00 Sleep" $ Just (Timestamp 22 0, "Sleep")

    it "should parse an entry ending in a comment" $ do
      test "08:00 Breakfast -- should I try skippin bfast?" $ Just (Timestamp 8 0, "Breakfast")

  describe "parseDateLine" $ do
    let test = runTest parseDateLine

    it "should parse a bare date line" $ do
      test "# 2025-02-05" $ Just (Date 2025 2 5)

    it "should parse a date line ending in a newline" $ do
      test "# 2025-02-05\n" $ Just (Date 2025 2 5)

    it "should parse a date line ending in a comment and eof" $ do
      test "# 2025-02-07 -- dates not necessarily sequential" $ Just (Date 2025 2 7)

    it "should parse a date line ending in a comment and newline" $ do
      test "# 2025-02-07 -- dates not necessarily sequential\n" $ Just (Date 2025 2 7)

    it "should not consume a newline after the date" $ do
      let dateAndNewline = do
            date <- parseDateLine
            newline
            return date
      runTest dateAndNewline "# 2025-02-05\n" $ Just (Date 2025 2 5)

  describe "parseDayList" $ do
    let test = runTest parseDayList

    it "should parse a day list with one entry not ending in newline" $ do
      test "# 2025-02-05\n22:00 Sleep" $ Just (Date 2025 2 5, [(Timestamp 22 0, "Sleep")])

    it "should parse a day list with one entry ending in newline" $ do
      test "# 2025-02-05\n22:00 Sleep\n" $ Just (Date 2025 2 5, [(Timestamp 22 0, "Sleep")])

    it "should parse a day list with more than one entry not ending in newline" $ do
      let date = "# 2025-02-05"
      let activity1 = "22:00 Sleep -- comment"
      let activity2 = "22:30 More sleep"
      let expected = (Date 2025 2 5, [(Timestamp 22 0, "Sleep"), (Timestamp 22 30, "More sleep")])
      test (intercalate "\n" [date, activity1, activity2]) (Just expected)

  describe "parseLog" $ do
    let test = runTest parseLog

    it "should parse a basic log" $ do
      let date1 = "# 2025-02-05"
      let activity1 = "22:00 Sleep -- comment"
      let activity2 = "22:30 More sleep\n"
      let day1 = intercalate "\n" [date1, activity1, activity2]
      let date2 = "# 2025-02-06"
      let activity3 = "10:00 Wake up"
      let activity4 = "11:00 Go back to sleep"
      let day2 = intercalate "\n" [date2, activity3, activity4]
      let log = intercalate "\n" [day1, day2]
      let expected =
            M.fromList
            [ (Date 2025 2 5, [(Timestamp 22 0, "Sleep"), (Timestamp 22 30, "More sleep")])
            , (Date 2025 2 6, [(Timestamp 10 0, "Wake up"), (Timestamp 11 0, "Go back to sleep")])
            ]
      test log (Just expected)

    it "should parse a log with a comment line" $ do
      let date1 = "# 2025-02-05"
      let activity1 = "22:00 Sleep -- comment"
      let activity2 = "22:30 More sleep\n"
      let log = intercalate "\n" ["-- hello", "", date1, activity1, activity2, "-- another one"]
      let expected =
            M.fromList [ (Date 2025 2 5, [(Timestamp 22 0, "Sleep"), (Timestamp 22 30, "More sleep")]) ]
      test log (Just expected)

    it "should parse a log with comments inside the day lists" $ do
      let date1 = "# 2025-02-05"
      let activity1 = "22:00 Sleep -- comment"
      let activity2 = "22:30 More sleep\n"
      let day1 = intercalate "\n" [date1, activity1, "-- comment", activity2]
      let date2 = "# 2025-02-06"
      let activity3 = "10:00 Wake up"
      let activity4 = "11:00 Go back to sleep"
      let day2 = intercalate "\n" [date2, "-- another comment", activity3, activity4]
      let log = intercalate "\n" [day1, day2]
      let expected =
            M.fromList
            [ (Date 2025 2 5, [(Timestamp 22 0, "Sleep"), (Timestamp 22 30, "More sleep")])
            , (Date 2025 2 6, [(Timestamp 10 0, "Wake up"), (Timestamp 11 0, "Go back to sleep")])
            ]
      test log (Just expected)
