module MonadTransformersExercises where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

-- using Identity explicitly for clarity
rDec :: Num a => Reader a a
rDec = ReaderT $ Identity . (subtract 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
  putStrLn $ "Hi: " ++ show r
  return $ r + 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
  let str = show s
  putStrLn $ "Hi: " ++ str
  return $ (str, s + 1)

isValid :: String -> Bool
isValid v = '!' `elem` v

-- lift getLine into the MaybeT context
maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

-- use runMaybeT to get the IO (Maybe String)
-- out of the MaybeT IO String from maybeExcite
doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e -> putStrLn ("Good, was very excite: " ++ e)
