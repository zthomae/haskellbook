module WordNumber where

-- intercalate is like concat . intersperse
import Data.List (intercalate)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = ""

digits :: Int -> [Int]
digits 0 = [0]
digits n = go n []
  where go num lst
         | num == 0 = lst
         | otherwise = go (div num 10) $ mod num 10 : lst

wordNumber :: Int -> String
wordNumber n = intercalate "-" (map digitToWord (digits n))
