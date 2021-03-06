{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module SemigroupExercises where

import GHC.Generics
import Data.Semigroup
import Test.QuickCheck hiding (Failure, Success)

-- this is terrible
import Text.Show.Functions

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

--

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three a b c) <> (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool

--

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (Four a b c d) <> (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool

--

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

--

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _ <> _ = BoolDisj True

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--

data Or a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  s@(Snd _) <> _ = s
  _ <> s@(Snd _) = s
  _ <> f = f

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

type OrAssoc = Or String String -> Or String String -> Or String String -> Bool

--

newtype Combine a b = Combine { unCombine :: (a -> b) } deriving (Generic, Show)

instance (Semigroup b) => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ \a -> (f a) <> (g a)

instance CoArbitrary (Combine String String)

combineAssoc :: (Eq a, Semigroup a) => a -> Combine a a -> Combine a a -> Combine a a -> Bool
combineAssoc a f g h =
  let
    (Combine left) = (f <> g) <> h
    (Combine right) = f <> (g <> h)
  in
    left a == right a

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = do
    f <- arbitrary
    return $ Combine f

type CombineAssoc = Combine String String -> Combine String String -> Combine String String -> Bool

--

newtype Comp a = Comp { unComp :: (a -> a) } deriving (Generic, Show)

instance (Semigroup a) => Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f . g)

--instance CoArbitrary (Comp String)

-- this is repetitive
compAssoc :: (Eq a, Semigroup a) => a -> Comp a -> Comp a -> Comp a -> Bool
compAssoc a f g h =
  let
    (Comp left) = (f <> g) <> h
    (Comp right) = f <> (g <> h)
  in
    left a == right a

instance (Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
  arbitrary = do
    f <- arbitrary
    return $ Comp f

type CompAssoc = Comp String -> Comp String -> Comp String -> Bool

--

data Validation a b = Failure a | Success b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Failure b = Failure (a <> b)
  left@(Failure a) <> _ = left
  _ <> right@(Failure a) = right
  _ <> right = right

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Failure a, Success b]

type ValidationAssoc = Validation String String -> Validation String String -> Validation String String -> Bool

--

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight (Success (a <> b))
  _ <> right@(AccumulateRight (Failure a)) = right
  left@(AccumulateRight (Failure a)) <> _ = left

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateRight (Failure a), AccumulateRight (Success b)]

type AccumulateRightAssoc = AccumulateRight String String -> AccumulateRight String String -> AccumulateRight String String -> Bool

--

newtype AccumulateBoth a b =
  AccumulateBoth (Validation a b)
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Success a) <> AccumulateBoth (Success b) = AccumulateBoth (Success (a <> b))
  AccumulateBoth (Failure a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure (a <> b))
  _ <> right@(AccumulateBoth (Failure a)) = right
  left@(AccumulateBoth (Failure a)) <> _ = left

instance (Arbitrary a, Arbitrary b) => Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [AccumulateBoth (Failure a), AccumulateBoth (Success b)]

type AccumulateBothAssoc = AccumulateBoth String String -> AccumulateBoth String String -> AccumulateBoth String String -> Bool

main :: IO ()
main = do
  putStrLn "Trivial tests"
  quickCheck (semigroupAssoc :: TrivialAssoc)

  putStrLn "Identity tests"
  quickCheck (semigroupAssoc :: IdentityAssoc)

  putStrLn "Two tests"
  quickCheck (semigroupAssoc :: TwoAssoc)

  putStrLn "Three tests"
  quickCheck (semigroupAssoc :: ThreeAssoc)

  putStrLn "Four tests"
  quickCheck (semigroupAssoc :: FourAssoc)

  putStrLn "BoolConj tests"
  quickCheck (semigroupAssoc :: BoolConjAssoc)

  putStrLn "BoolDisj tests"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)

  putStrLn "Or tests"
  quickCheck (semigroupAssoc :: OrAssoc)

  putStrLn "Combine tests"
  quickCheck ((combineAssoc "a") :: CombineAssoc)

  putStrLn "Comp tests"
  quickCheck (compAssoc "a")

  putStrLn "Validation tests"
  quickCheck (semigroupAssoc :: ValidationAssoc)

  putStrLn "AccumulateRight tests"
  quickCheck (semigroupAssoc :: AccumulateRightAssoc)

  putStrLn "AccumulateBoth tests"
  quickCheck (semigroupAssoc :: AccumulateBothAssoc)
