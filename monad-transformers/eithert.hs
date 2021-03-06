{-# LANGUAGE InstanceSigs #-}
module EitherTransformer where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) =
    EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT (pure (pure x))

  (EitherT fab) <*> (EitherT me) =
    EitherT $ (<*>) <$> fab <*> me

-- I'm not winning any style points with this pattern matching...

instance Monad m => Monad (EitherT e m) where
  return = pure

  (EitherT ma) >>= f =
    EitherT $ do
      v <- ma
      case v of
        Left e -> return $ Left e
        Right a -> runEitherT (f a)

swapEither :: Either e a -> Either a e
swapEither v = case v of
  Left e -> Right e
  Right a -> Left a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT m) = EitherT $ fmap swapEither m

eitherT :: Monad m =>
           (a -> m c)
        -> (b -> m c)
        -> EitherT a m b
        -> m c
eitherT fa fb (EitherT m) = do
  v <- m
  case v of
    Left a -> fa a
    Right b -> fb b

-- this instance is almost identical to that
-- of MaybeT. I also explicitly use Right
-- instead of return, for clarity of what
-- this does (although you could perhaps
-- tell given that the left type is part
-- of the structure MonadTrans is defined
-- on).
instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

instance (MonadIO m) => MonadIO (EitherT e m) where
  liftIO = lift . liftIO
