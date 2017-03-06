module LearnParsers where

import Control.Applicative
import Text.Parser.Combinators (eof)
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser a
one' = one >> stop

one'' = one >> eof >> stop

-- char :: Char -> Parser Char
-- char c =
--   Parser $ \ s ->
--              case s of
--                (x:xs) -> if c == x
--                          then [(c, xs)]
--                          else []
--                _ -> []

oneTwo :: Parser Char -- needed to disambiguate
oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwo'' = oneTwo >> eof >> stop

testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseWithInput :: Show a => Parser a -> String -> IO ()
testParseWithInput p s =
  print $ parseString p mempty s

-- I don't exactly know what the exercise wants me to do, so here's one guess implemented
-- with things we haven't learned yet...
oneTwoThree :: Parser String
oneTwoThree = string "123" <|> string "12" <|>  string "1"

string' :: String -> Parser String
string' [] = stop
string' (x:xs) = char x >> string' xs

pNL s = putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop -- "123" can't be parsed as stop
  pNL "one:"
  testParse one -- "123" can be partially parsed as a 1
  pNL "one':"
  testParse one' -- "123" can't be parsed because we reach stop
  pNL "one'':"
  testParse one'' -- "123" can't be parsed as a 1 followed by an eof
  pNL "oneTwo:"
  testParse oneTwo -- "123" can be partially parsed as a 1 and then a 2
  pNL "oneTwo':"
  testParse oneTwo' -- "123" can't be parsed because we reach stop
  pNL "oneTwo'':"
  testParse oneTwo'' -- "123" can't be parsed as a 1, a 2, and an eof
  pNL "oneTwoThree:"
  testParseWithInput oneTwoThree "1" -- oneTwoThree can parse a "1"
  testParseWithInput oneTwoThree "12" -- oneTwoThree can parse a "12"
  testParseWithInput oneTwoThree "123" -- oneTwoThree can parse a "123"
  testParseWithInput oneTwoThree "23" -- oneTwoThree can't parse a "23"
  testParseWithInput (string' "123") "123" -- (string' "123") can parse a "123"
  testParseWithInput (string' "123") "12" -- (string' "123") can't parse a "12"
