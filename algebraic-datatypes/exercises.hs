module Ch11Exercises where

import Data.Char
import Data.List
import Data.Foldable

---
-- Vigenere
---

-- Note: only works on lowercase letters (unlike book...)

encode :: (Char, Char) -> Char
encode (b, c) =
  let a = ord 'a'
  in chr $ a + (ord c + ord b - (2 * a)) `mod` 26

-- Not splitting into words first
vigenereCipher :: String -> String -> String
vigenereCipher key plaintext = map encode (zip (cycle key) plaintext)

-- Let's handle words

vigenereWords :: String -> String -> String
vigenereWords key plaintext = intercalate " " (map (map encode) $ zipWords (words plaintext) (cycle key))

-- Dear God...
zipWords :: [[a]] -> [a] -> [[(a, a)]]
zipWords [] _ = []
zipWords (w:ws) seq = zip w (take n seq) : zipWords ws (drop n seq) where n = length w

---
-- as patterns
--

myIsSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
myIsSubsequenceOf [] _ = True
myIsSubsequenceOf (s:ss) [] = False -- Necessary to qualify seq?
myIsSubsequenceOf seq@(s:ss) (l:ls)
  | s == l = myIsSubsequenceOf ss ls
  | otherwise = myIsSubsequenceOf seq ls

-- Not a fan of this arrangement...

capitalizeWords s = capitalizeWords' (words s)

capitalizeWords' :: [String] -> [(String, String)]
capitalizeWords' (w@(a:as):ws) = (w, (toUpper a):as) : capitalizeWords' ws
capitalizeWords' _ = []

---
-- Language exercises
---

-- Could have used this above...
capitalizeWord :: String -> String
capitalizeWord "" = ""
capitalizeWord (c:cs) = (toUpper c):cs

-- TODO: Iterate more cleanly to get rid of some of these reversals
sentences :: String -> [String]
sentences = reverse . map reverse . fst . foldl' aux ([], True)
  where addCharToSentences c [] = [[c]]
        addCharToSentences c (w:ws) = (c:w):ws
        aux (acc, isInSentence) next =
          case next of
            '.' -> (addCharToSentences '.' acc, False)
            ' ' -> if isInSentence
                   then (addCharToSentences ' ' acc, True)
                   else (acc, False)
            _ -> if isInSentence
                 then (addCharToSentences next acc, True)
                 else ([next]:acc, True)

capitalizeParagraph :: String -> String
capitalizeParagraph p = intercalate " " (map capitalizeWord (sentences p))

---
-- Phone exercise
---

-- first arg is the list of strings on number keys, second arg is the list of chars on pound key
-- (assuming that * only does capitalization...)
-- note: I did not make a very good data structure
data DaPhone = DaPhone { daPhoneNums :: [String] , daPhonePound :: String }

phone :: DaPhone
phone = DaPhone { daPhoneNums = ["+_", "", "abc", "def", "ghi", "jkl", "mno", "pqrs", "tuv", "wxyz"]
                , daPhonePound = ".," }

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol lol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Haha thanks just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

type Presses = Int

-- Going to modify the types given to use Maybe and in other ways that make the exercises simpler

findIndexInner :: Eq a => a -> [[a]] -> Maybe (Int, Int)
findIndexInner e lists = go 0 lists
  where go ind ls = case ls of
                      [] -> Nothing
                      l:ls -> case (elemIndex e l) of
                                Just i -> Just (ind, i)
                                _ -> go (ind + 1) ls

reverseTaps :: DaPhone -> Char -> Maybe [(Digit, Presses)]
reverseTaps config char
  | isUpper char = fmap (\keys -> ('*', 1) : keys) (reverseTaps config (toLower char))
  | otherwise = fmap (\(key, times) -> [(intToDigit key, times + 1)]) (findIndexInner char (daPhoneNums config))
-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]

cellPhonesDead :: DaPhone -> String -> Maybe [(Digit, Presses)]
cellPhonesDead config msg = fmap concat (sequence (map (reverseTaps phone) msg))

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\ (_, presses) -> (+presses)) 0 -- this might be a bit too far into PFS

-- This is probably not expressed well, but I wanted it to behave predictably wrt ties...
-- A better solution might be to write a different function that returns a list of most popular characters
mostPopularLetter :: Ord a => [a] -> (a, Int)
mostPopularLetter s@(c:_) = snd $ foldr go ((c, 0), (c, 0)) (sort s)
  where go next (current@(c, count), best@(_, most))
          | next == c = if count + 1 >= most then ((c, count + 1), (c, count + 1)) else ((c, count + 1), best)
          | otherwise = ((next, 1), best)

coolestLtr :: String -> (Char, Int)
coolestLtr msg = mostPopularLetter (concat (words msg))

-- lol check this out
coolestWord :: [String] -> (String, Int)
coolestWord = mostPopularLetter

---
-- Hutton's Razor
---

data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval e = case e of
  Lit i -> i
  Add e1 e2 -> (eval e1) + (eval e2)

printExpr :: Expr -> String
printExpr e = case e of
  Lit i -> show i
  Add e1 e2 -> concat [printExpr e1, " + ", printExpr e2]
