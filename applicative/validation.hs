import Control.Applicative
import Data.Monoid
import Test.QuickCheck hiding (Failure, Success)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
  Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance Monoid e => Applicative (Validation e) where
  pure x = Success x
  (Failure a) <*> (Failure b) = Failure (mappend a b)
  (Failure a) <*> _ = Failure a
  _ <*> (Failure b) = Failure b
  (Success f) <*> x = fmap f x

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary =
    frequency [ (1, Failure <$> arbitrary)
              , (1, Success <$> arbitrary) ]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

main :: IO ()
main = do
  quickBatch $ applicative (undefined :: Validation String (String, String, Integer))
