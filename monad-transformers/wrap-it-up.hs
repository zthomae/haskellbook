module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

-- we need to transform out result into a function awaiting a
-- argument and wrapping in a monadic structure (in this case, IO)
-- before passing it to ReaderT
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT (ExceptT (ReaderT $ return <$> (const (Right (Just 1)))))
