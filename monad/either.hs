module EitherMonad where

import Control.Applicative
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
  pure x = Second x

  (Second f) <*> (Second b) = Second (f b)
  (Second _) <*> First a = First a
  (First a) <*> _ = First a

instance Monad (Sum a) where
  return = pure

  Second b >>= f = f b
  First a >>= _ = First a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [(1, First <$> arbitrary), (1, Second <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch (monad (undefined :: Sum String (String, String, String)))
