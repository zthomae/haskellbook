module MonoidExercises where

import Data.Semigroup
import Test.QuickCheck

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = mappend mempty x == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = mappend x mempty == x

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  _ <> _ = Trivial

instance Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivialAssoc = Trivial -> Trivial -> Trivial -> Bool

--

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance (Semigroup a, Monoid a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

--

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')

instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

--

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (<>)

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

instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (<>)

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

--

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance (Semigroup b) => Semigroup (Combine a b) where
  (<>) = undefined

instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty = undefined
  mappend = (<>)

--

newtype Comp a = Comp { unComp :: (a -> a) }

instance (Semigroup a) => Semigroup (Comp a) where
  (<>) = undefined

instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty = undefined
  mappend = (<>)

--

newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem $ \s ->
    let (a, s') = runMem (Mem g) s
        (b, s'') = runMem (Mem f) s'
    in (a <> b, s'')

instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)

type MemAssoc = Mem String String -> Mem String String -> Mem String String -> Bool

memF = Mem $ \s -> ("hi", s + 1)

main :: IO ()
main = do
  putStrLn "Trivial tests"
  quickCheck (semigroupAssoc :: TrivialAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)

  putStrLn "Identity tests"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)

  putStrLn "Two tests"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)

  putStrLn "BoolConj tests"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)

  putStrLn "BoolDisj tests"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)

  putStrLn "Mem tests"
  print $ runMem (memF <> mempty) 0 == ("hi", 1)
  print $ runMem (mempty <> memF) 0 == ("hi", 1)
  print $ (runMem mempty 0 :: (String, Int)) == (mempty, 0)
  print $ runMem (memF <> mempty) 0 == runMem memF 0
  print $ runMem (mempty <> memF) 0 == runMem memF 0
