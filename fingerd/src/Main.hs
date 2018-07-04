{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forever)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (bindSocket)

import Finger.Model (User(..))
import Finger.Network (bindSocket, receive, respondWith)
import Finger.Queries (getUser, getUsers)

columnStart :: Int -> Int -> Int -> Int
columnStart target gapSize len =
  let
    minCol = max target len
  in
    if minCol - len < gapSize
    then minCol + 8
    else minCol

spacesAfter :: Int -> Int -> Int -> Int -> (Int, Int)
spacesAfter target gapSize first second
    | first < second =
      let (a, b) = spacesAfter target gapSize second first in (b, a)
    | otherwise =
      let
        start = columnStart target gapSize first
      in
        (start - first, start - second)

tabsWithPadding :: Int -> ByteString
tabsWithPadding len = BS.concat
  [ BS.replicate (len `mod` 8) (toEnum (fromEnum ' '))
  , BS.replicate (len `div` 8) (toEnum (fromEnum '\t'))
  ]

formatUser :: User -> ByteString
formatUser user =
  let
    login = BS.concat ["Login: ", encodeUtf8 (username user)]
    dir = BS.concat ["Directory: ", encodeUtf8 (homeDirectory user)]
    (spaceAfterLogin, spaceAfterDir) = spacesAfter 40 8 (BS.length login) (BS.length dir)
  in
    BS.concat
    [ login, tabsWithPadding spaceAfterLogin
    , "Name: ", encodeUtf8 (realName user), "\n"
    , dir, tabsWithPadding spaceAfterDir
    , "Shell: ", encodeUtf8 (shell user), "\n"
    ]

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- receive soc
  resp <- case msg of
    "\r\n" -> do
      rows <- getUsers dbConn
      let usernames = map username rows
          newlineSeparated = T.concat $ intersperse "\n" usernames
      return $ Just (encodeUtf8 newlineSeparated)
    name -> (getUser (decodeUtf8 name) dbConn) >>= (return . ((<$>) formatUser))
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
