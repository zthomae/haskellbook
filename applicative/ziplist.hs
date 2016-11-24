module Apl1 where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- unfortunate orphan instances. Try to avoid these
-- in code you're going to keep or release.

-- this isn't going to work properly
instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList []
  mappend = liftA2 mappend

instance Arbitrary a => Arbitrary (ZipList a) where
  arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Sum a) where
  arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure x = Cons x Nil
  fs <*> xs = flatMap (\f -> fmap f xs) fs

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' 0 _ = Nil
take' i (Cons car cdr) = Cons car (take' (i - 1) cdr)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

-- this EqProp instance only checks the first 100 values
-- of each list instead of failing to halt for infinitely-long
-- lists
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 100 l
          ys' = let (ZipList' l) = ys
                in take' 100 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

zip' :: List a -> List b -> List (a, b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons a as) (Cons b bs) = Cons (a, b) $ zip' as bs

repeat' :: a -> List a
repeat' a = Cons a (repeat' a)

instance Applicative ZipList' where
  pure x = ZipList' $ repeat' x
  (ZipList' fs) <*> (ZipList' xs) = ZipList' $ fmap (uncurry ($)) (zip' fs xs)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return Nil)
              , (2, Cons <$> arbitrary <*> arbitrary) ]

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

main :: IO ()
main = do
  quickBatch $ functor (undefined :: ZipList' (String, String, Integer))
  quickBatch $ applicative (undefined :: ZipList' (String, String, Integer))
