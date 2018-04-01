{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.Text.Encoding (decodeUtf8)
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (bindSocket)

import Finger.Network
import Finger.Service

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- receive soc
  resp <- case msg of
    "\r\n" -> returnUsers dbConn
    name -> returnUser (decodeUtf8 name) dbConn
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
