module FoldableExercises where

import Data.Foldable
import Data.Monoid

data Constant a b = Constant a

-- the only value given to a Constant is part of the
-- structure. all that remains is empty.
instance Foldable (Constant a) where
  foldMap _ _ = mempty

data Two a b = Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

-- I think these make sense...
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' _ x y) = (f x) <> (f y)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' _ x y z) = (f x) <> (f y) <> (f z)

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF pred f = foldMap (\ a -> if pred a then pure a else mempty) f
