module Ch12Exercises where

import Data.Char
import Data.List

---
-- String processing
---

notThe :: String -> Maybe String
notThe s = if (s == "the") then Nothing else Just s

replaceThe :: String -> String
replaceThe w = intercalate " " (map (fromMaybe "a" . notThe) (words w))

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s =
  let
    ws = words s
    startsWithVowel s = isVowel (head s)
    isTheBeforeVowel = \ (first, second) ->
      case notThe first of
        Nothing -> startsWithVowel second
        _ -> False
  in
    genericLength $ filter isTheBeforeVowel (zip ws (tail ws))

vowels = "aeiou"

isVowel :: Char -> Bool
isVowel c = elem c vowels

countVowels :: String -> Integer
--countVowels s = genericLength (filter isVowel s)
countVowels = foldr (\ char count -> if (isVowel char) then count + 1 else count) 0
---
-- Validate the word
---

newtype Word' =
  Word' String
  deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord s =
  if (numVowels > numConsonants) then Nothing else Just (Word' s)
  where (numVowels, numConsonants) =
          foldr
            (\ char (vs, cs) -> if (isVowel char) then (vs + 1, cs) else (vs, cs + 1))
            (0, 0)
            (map toLower s)

---
-- It's only Natural
---

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger n = case n of
  Zero -> 0
  Succ n' -> 1 + natToInteger n'

integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i == 0 = Just Zero
  | i > 0 = Just $ Succ (fromMaybe Zero $ integerToNat (i - 1))
  | otherwise = Nothing
---
-- Small library for Maybe
---

isJust :: Maybe a -> Bool
isJust m = case m of
  Just _ -> True
  _ -> False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee alt f m = case m of
  Just a -> f a
  _ -> alt

fromMaybe :: a -> Maybe a -> a
fromMaybe alt m = case m of
  Just a -> a
  _ -> alt

listToMaybe :: [a] -> Maybe a
listToMaybe l = case l of
  [] -> Nothing
  x:_ -> Just x

maybeToList :: Maybe a -> [a]
maybeToList m = case m of
  Just a -> [a]
  _ -> []

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr (\ new rest -> case new of
                                   Just s -> s:rest
                                   _ -> rest) []
-- catMaybes ms = [a | Just a <- ms]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr (\ new rest -> case (new, rest) of
                                   (_, Nothing) -> Nothing
                                   (Just s, Just rs) -> Just (s:rs)
                                   (_, Just _) -> rest) (Just [])

---
-- Small library for Either
---

lefts' :: [Either a b] -> [a]
lefts' = foldr (\ new rest -> case new of
                                Left a -> a:rest
                                _ -> rest) []
-- lefts' es = [e | Left e <- es]

rights' :: [Either a b] -> [b]
rights' = foldr (\ new rest -> case new of
                                 Right b -> b:rest
                                 _ -> rest) []
-- rights' es = [e | Right e <- es]

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' es = (lefts' es, rights' es)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
  Right b -> Just (f b)
  _ -> Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fa fb e = case e of
  Left a -> fa a
  Right b -> fb b

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f e = either' (\_ -> Nothing) (Just . f) e

---
-- Unfolds
---

myIterate :: (a -> a) -> a -> [a]
myIterate f start = start : myIterate f (f start)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f start = case f start of
  Just (a, b) -> a : myUnfoldr f b
  _ -> []

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\a -> Just (a, f a)) x

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
-- unfold f start = case f start of
--   Just (left, root, right) -> Node (unfold f left) root (unfold f right)
--   _ -> Leaf
unfold f start =
  foldr (\(left, root, right) _ -> Node (unfold f left) root (unfold f right)) Leaf (f start)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\i -> if i >= n then Nothing else Just (i+1, i, i+1)) 0
