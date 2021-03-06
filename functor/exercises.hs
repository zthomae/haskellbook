{-# LANGUAGE FlexibleInstances #-}

module FunctorExercises where

import GHC.Arr

-- you cannot write a Functor instance of the following, because
-- it has the kind *
data Bool' = False | True

data BoolAndSomethingElse a =
  False' a | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

data BoolAndMaybeSomethingElse a = Falsish |  Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap f (Truish a) = Truish (f a)
  fmap _ _ = Falsish

-- Mu has kind (* -> *) -> *. You can't write a functor for it.
newtype Mu f = InF { outF :: f (Mu f) }

-- D has the kind *, so you cannot write a Functor
-- instance for it
data D = D (Array Word Word) Int Int

data Sum a b = First a | Second b

-- The instance given did First (f a) and Second b. These
-- need to be flipped, because the First type is part of
-- the structure
instance Functor (Sum e) where
  fmap f (First a) = First a
  fmap f (Second b) = Second (f b)

data Company a b c = DeepBlue a c | Something b

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

data More a b = L a b a | R b a b

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

data Quant a b = Finance | Desk a | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

data K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype K' a b = K' a

instance Functor (Flip K' a) where
  fmap f (Flip (K' a)) = Flip $ K' (f a)

data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

data LiftItOut f a = LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)

data Parappa f g a = DaWrappa (f a) (g a) deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa one two) = DaWrappa (fmap f one) (fmap f two)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething one two) = IgnoringSomething one (fmap f two)

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious o a t) = Notorious o a (fmap f t)

data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons car cdr) = Cons (f car) (fmap f cdr)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats one two three) = MoreGoats (fmap f one) (fmap f two) (fmap f three)

data TalkToMe a = Halt | Print String a | Read (String -> a)

getPrint :: TalkToMe a -> Maybe a
getPrint (Print _ a) = Just a
getPrint _ = Nothing

getReadFunc :: TalkToMe a -> Maybe (String -> a)
getReadFunc (Read f) = Just f
getReadFunc _ = Nothing

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s (f a)
  -- identical to fmap f (Read func) = Read (f . func)
  fmap f (Read func) = Read (fmap f func)
