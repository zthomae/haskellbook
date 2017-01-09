{-# LANGUAGE InstanceSigs #-}
module StateTransformer where

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sm) = StateT $ \s -> (\(a, s1) -> (f a, s1)) <$> sm s

-- you need the Monad constraint because you're threading the
-- state through in order
instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (StateT f) <*> (StateT a) = undefined

instance Monad m => Monad (StateT s m) where
  return = pure

  sma >>= f = undefined
