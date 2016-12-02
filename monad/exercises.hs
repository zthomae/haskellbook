module MonadExercises where

import Control.Applicative
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

---- Monad instances

-- 1
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

-- 2 (renamed)
data LeftEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (LeftEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (LeftEither b) where
  pure = Left'
  (Left' f) <*> x = fmap f x
  (Right' v) <*> _ = Right' v

instance Monad (LeftEither b) where
  return = pure
  (Left' v) >>= f = f v
  (Right' v) >>= _ = Right' v

instance (Arbitrary a, Arbitrary b) => Arbitrary (LeftEither b a) where
  arbitrary = frequency [ (1, return Left' <*> arbitrary)
                        , (1, return Right' <*> arbitrary)]

instance (Eq a, Eq b) => EqProp (LeftEither b a) where
  (=-=) = eq

-- 3
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> v = fmap f v

instance Monad Identity where
  return = pure
  (Identity v) >>= f = f v

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq

-- 4
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

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  xs >>= f = concat' $ fmap f xs -- could use monoid instead

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    frequency [ (1, return Nil)
              , (2, Cons <$> arbitrary <*> arbitrary) ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

---- Functions on monads

j :: Monad m => m (m a) -> m a
j m = m >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = a >>= (return . f)

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= \a -> b >>= \b -> return $ f a b

a :: Monad m => m a -> m (a -> b) -> m b
a m f = f >>= (\g -> (m >>= (return . g)))

-- hint: you'll need recursion
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = return []
-- this doesn't feel great
meh (a:as) f = do
  car <- f a
  cdr <- meh as f
  return (car : cdr)

-- hint: use "meh"
flipType :: Monad m => [m a] -> m [a]
flipType l = fmap concat (meh l (\ ma -> ma >>= (\ a -> return [a])))

type SSS = (String, String, String)
main :: IO ()
main = do
  quickBatch $ monad (undefined :: Nope SSS)
  quickBatch $ monad (undefined :: LeftEither String SSS)
  quickBatch $ monad (undefined :: Identity SSS)
  quickBatch $ monad (undefined :: List SSS)
