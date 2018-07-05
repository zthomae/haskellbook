{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (bindSocket)

import Finger.Display (formatUser)
import Finger.Model (User(..))
import Finger.Network (bindSocket, receive, respondWith)
import Finger.Queries (getUser, getUsers)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- receive soc
  resp <- case msg of
    "\r\n" -> do
      rows <- getUsers dbConn
      let usernames = map username rows
          newlineSeparated = T.concat $ intersperse "\n" usernames
      return $ Just (encodeUtf8 newlineSeparated)
    name -> (getUser (decodeUtf8 name) dbConn) >>= \user -> return $ encodeUtf8 <$> formatUser <$> user
  respondWith resp soc

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

main :: IO ()
main = withSocketsDo $ do
  sock <- bindSocket 79
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  close sock
