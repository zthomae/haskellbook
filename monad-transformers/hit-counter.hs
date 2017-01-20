{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.lookup k m of
    Just v -> (M.insert k (v + 1) m, v + 1)
    Nothing -> (M.insert k 1 m, 1)

param' :: Text -> ActionT Text (ReaderT Config IO) Text
param' k = rescue (param k) (const (return "the parameter was missing"))

lookupOrInsert :: Ord a => a -> b -> (b -> b) -> M.Map a b -> M.Map a b
lookupOrInsert key dflt next map =
  case (M.lookup key map) of
    Just val -> M.adjust next key map
    Nothing -> M.insert key dflt map

updateKeyCount :: Config -> Text -> IO Integer
updateKeyCount config key = do
  let ref = counts config
  originalMap <- readIORef ref
  let updatedMap = lookupOrInsert key 1 (+1) originalMap
  writeIORef ref updatedMap
  return $ updatedMap M.! key

app :: Scotty ()
app =
  get "/:key" $ do
    config <- lift ask
    unprefixed <- param' "key"
    let key' = mappend (prefix config) unprefixed
    newInteger <- liftIO $ updateKeyCount config key'
    html $ mconcat [ "<h1>Success! count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR = flip runReaderT config
  scottyT 3000 runR app
