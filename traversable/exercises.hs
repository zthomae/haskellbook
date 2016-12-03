module TraversableExercises where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers

-- Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

-- Constant

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ _ = mempty

instance Traversable (Constant a) where
  traverse f (Constant a) = Constant <$> pure a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
  (=-=) = eq

-- Maybe
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure Nada
  traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [ (1, Yep <$> arbitrary)
                        , (1, return Nada)
                        ]

instance Eq a => EqProp (Optional a) where
  (=-=) = eq

-- List
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil

  mappend Nil Nil = Nil
  mappend a Nil = a
  mappend Nil b = b
  mappend a b = append a b

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons car cdr) = f car <> foldMap f cdr

instance Applicative List where
  pure x = Cons x Nil
  (Cons f fs) <*> l = append (fmap f l) (fs <*> l)

instance Traversable List where
  traverse _ Nil = pure Nil
  traverse f (Cons car cdr) = undefined

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (1, Cons <$> arbitrary <*> arbitrary)
                        , (1, return Nil)
                        ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

-- Three
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

-- Three'
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' a b b') = f b <> f b'

instance Traversable (Three' a) where
  traverse f (Three' a b b') = (Three' a) <$> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = liftA3 Three' arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

-- S
data S n a = S (n a) a

instance Functor (S n) where
  fmap f (S n a) = undefined

instance Foldable (S n) where
  foldMap f (S n a) = undefined

instance Traversable n => Traversable (S n) where
  traverse = undefined

-- Tree
data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) <> f a <> (foldMap f right)

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = undefined

type TY = (Int, Int, [Int])
main :: IO ()
main = do
  quickBatch (traversable (undefined :: Identity TY))
  quickBatch (traversable (undefined :: Constant Int TY))
  quickBatch (traversable (undefined :: Optional TY))
  --quickBatch (traversable (undefined :: List TY))
  quickBatch (traversable (undefined :: Three Int Int TY))
  quickBatch (traversable (undefined :: Three' Int TY))
  --quickBatch S
  --quickBatch Tree
