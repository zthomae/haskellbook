module Ch12Exercises where

import Data.List

---
-- String processing
---

notThe :: String -> Maybe String
notThe s = if (s == "the") then Nothing else Just s

replaceThe :: String -> String
replaceThe w = intercalate " " (map (\a -> case notThe(a) of
                                        Just s -> s
                                        Nothing -> "a") (words w))

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = undefined

countVowels :: String -> Integer
countVowels = undefined

---
-- Validate the word
---

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord = undefined

---
-- It's only Natural
---

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger = undefined

integerToNat :: Integer -> Maybe Nat
integerToNat = undefined

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

rights' :: [Either a b] -> [b]
rights' = foldr (\ new rest -> case new of
                                 Right b -> b:rest
                                 _ -> rest) []

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
myIterate = undefined

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr = undefined

betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr undefined undefined

data BinaryTree a
  = Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold = undefined

treeBuild :: Integer -> BinaryTree Integer
treeBuild = undefined
