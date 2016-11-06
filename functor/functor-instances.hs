module FunctorInstances where

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

-- 1
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

-- 2
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance (Arbitrary a) => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    return $ Pair a a'

-- 3
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

-- 4
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

-- 5
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Three' a b b'

-- 6
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

-- 7
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    a' <- arbitrary
    a'' <- arbitrary
    b <- arbitrary
    return $ Four' a a' a'' b

-- 8
data Trivial = Trivial

-- Trivial has the kind *, while Functor expects
-- the kind * -> *. Therefore, you cannot implement
-- a Functor instance for it

main :: IO ()
main = do
  putStrLn "Identity Tests"
  quickCheck $ \x -> functorIdentity (x :: Identity String)
  quickCheck (functorCompose :: Identity String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Pair tests"
  quickCheck $ \x -> functorIdentity (x :: Pair String)
  quickCheck (functorCompose :: Pair String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Two tests"
  quickCheck $ \x -> functorIdentity (x :: Two String String)
  quickCheck (functorCompose :: Two String String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Three tests"
  quickCheck $ \x -> functorIdentity (x :: Three String String String)
  quickCheck (functorCompose :: Three String String String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Three' tests"
  quickCheck $ \x -> functorIdentity (x :: Three' String String)
  quickCheck (functorCompose :: Three' String String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Four tests"
  quickCheck $ \x -> functorIdentity (x :: Four String String String String)
  quickCheck (functorCompose :: Four String String String String -> Fun String String -> Fun String String -> Bool)

  putStrLn "Four' tests"
  quickCheck $ \x -> functorIdentity (x :: Four' String String)
  quickCheck (functorCompose :: Four' String String -> Fun String String -> Fun String String -> Bool)
