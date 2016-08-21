module MaybeAnotherMonoid where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Gen (oneof)

-- hello copypasta

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [ return Nada
          , return $ Only a
          ]

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    return $ First' a

instance Monoid (First' a) where
  mempty = First' Nada
  mappend f@(First' (Only _)) (First' Nada) = f
  mappend (First' Nada) f@(First' (Only _)) = f
  mappend f@(First' (Only _)) (First' (Only _)) = f
  mappend _ _ = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: First' String -> Bool)
  quickCheck (monoidRightIdentity :: First' String -> Bool)
