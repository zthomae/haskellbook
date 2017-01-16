{-# LANGUAGE InstanceSigs #-}
module StateTransformer where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  fmap f (StateT sm) = StateT $ \s -> (\(a, s1) -> (f a, s1)) <$> sm s

-- you need the Monad constraint because you're threading the
-- state through in order
instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

  (StateT f) <*> (StateT a) =
    StateT $ \s -> do
      (fab, s1) <- f s
      (a, s2) <- a s1
      return (fab a, s2)

instance Monad m => Monad (StateT s m) where
  return = pure

  (StateT sma) >>= f =
    StateT $ \s -> do
      (a, s1) <- sma s
      runStateT (f a) s1

-- We know that the result of the state
-- function needs to be wrapped in a monadic
-- context. So we simply pass on the state
-- and the value inside the monadic structure
-- and construct a tuple that we `return'
-- back to the monadic structure
instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
    a <- m
    return (a, s)

-- a pattern is developing
instance (MonadIO m) => MonadIO (StateT s m) where
  liftIO = lift . liftIO
