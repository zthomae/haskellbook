{-# LANGUAGE InstanceSigs #-}
module ReaderTransformer where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

-- I've been very explicit with the lambdas this time

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ \r -> f <$> (rma r)
  -- in the book: fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ \r -> pure x
  -- in the book: pure a = ReaderT (pure (pure a))

  (ReaderT f) <*> (ReaderT a) =
    ReaderT $ \r -> (f r) <*> (a r)
  -- in the book: ReaderT $ (<*>) <$> f <*> a

instance Monad m => Monad (ReaderT r m) where
  return = pure

  -- remember: the point of reader is sharing a
  -- common input
  (ReaderT m) >>= f =
    ReaderT $ \r -> do
      v <- m r
      runReaderT (f v) r

-- const will lift a value into a functional
-- context. in this case, we know we need
-- a function taking an r and returning an
-- m a. Passing an m a to const will give
-- us this (since we know the monadic structure
-- is around the return type of the function
-- stored in the reader).
instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

-- this is also the same as the EitherT instance...
instance (MonadIO m) => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO
