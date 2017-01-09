{-# LANGUAGE InstanceSigs #-}
module ReaderTransformer where

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
