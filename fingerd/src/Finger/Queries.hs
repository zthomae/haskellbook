{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Finger.Queries where

import Control.Exception
import Data.Text (Text)
import Data.Typeable()
import Database.SQLite.Simple (Connection, query, query_)
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import Finger.Model (User)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser = "INSERT INTO users VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers = "SELECT * FROM users"

getUserQuery :: Query
getUserQuery = "SELECT * FROM users WHERE username = ?"

data DuplicateData = DuplicateData deriving (Eq, Show)

instance Exception DuplicateData

getUser :: Text -> Connection -> IO (Maybe User)
getUser username conn = do
  results <- query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

getUsers :: Connection -> IO [User]
getUsers dbConn = query_ dbConn allUsers
