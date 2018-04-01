{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Finger.Service where

import Control.Exception (try)
import Control.Monad.Except (ExceptT(..))
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple (Connection, SQLError, execute, execute_, query_)

import Finger.Model (UserRow, formatUser, username)
import Finger.Queries (allUsers, createUsers, getUser, insertUser)

addUser :: UserRow -> Connection -> ExceptT SQLError IO ()
addUser user dbConn = ExceptT (try $ execute dbConn insertUser user)

createDatabase :: Connection -> IO ()
createDatabase conn = execute_ conn createUsers

returnUsers :: Connection -> IO (Maybe ByteString)
returnUsers dbConn = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  return $ Just (encodeUtf8 newlineSeparated)

returnUser :: Text -> Connection -> IO (Maybe ByteString)
returnUser username dbConn = do
  maybeUser <- getUser (T.strip username) dbConn
  return $ formatUser <$> maybeUser
