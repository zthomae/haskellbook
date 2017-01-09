{-# LANGUAGE InstanceSigs #-}

module CTInstances where

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (Identity f) <*> (Identity a) = Identity (f a)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) =>
         Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure a = Compose $ pure (pure a)

  -- This demands an explanation:
  -- (fmap (<*>))
  -- :: (Applicative f1, Functor f) =>
  --    f (f1 (a -> b)) -> f (f1 a -> f1 b)
  -- You map <*> over a f (g (a -> b)). This
  -- transforms the inner g (a -> b) into a
  -- g a -> g b, yielding type f (g a -> g b).
  -- We can do this because we know g is an
  -- applicative. And because f is an applicative
  -- as well, we can <*> this over an f (g a)
  -- to transform it into a f (g b).
  (<*>) :: Compose f g (a -> b)
        -> Compose f g a
        -> Compose f g b
  (Compose fgab) <*> (Compose fga) =
    Compose $ (<*>) <$> fgab <*> fga

instance (Foldable f, Foldable g) =>
         Foldable (Compose f g) where
  foldMap :: Monoid m => (a -> m) -> Compose f g a -> m
  foldMap f (Compose a) = (foldMap . foldMap) f a

instance (Traversable f, Traversable g) =>
         Traversable (Compose f g) where
  traverse :: Applicative f1 => (a -> f1 b) -> Compose f g a -> f1 (Compose f g b)
  traverse f (Compose a) = Compose <$> (traverse . traverse) f a
