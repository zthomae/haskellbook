module SumFunctor where

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

-- You cannot write a Functor instance that applies
-- only to First because the type a is bound first.
instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)
