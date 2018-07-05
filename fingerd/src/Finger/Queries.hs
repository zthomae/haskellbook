{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Finger.Queries where

import Control.Exception
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable()
import Database.SQLite.Simple (Connection, NamedParam((:=)), executeNamed, query, query_)
import Database.SQLite.Simple.Types
import Text.RawString.QQ

import Finger.Model (User(..))

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

updateUserQuery :: Query
updateUserQuery =
  "UPDATE users SET username = :username, shell = :shell, homeDirectory = :homeDirectory, realName = :realName, phone = :phone where id = :id"

data DuplicateData = DuplicateData deriving (Eq, Show)

instance Exception DuplicateData

getUser :: Text -> Connection -> IO (Maybe User)
getUser username conn = do
  results <- query conn getUserQuery (Only (T.strip username))
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn allUsers

updateUser :: User -> Connection -> IO ()
updateUser user conn = executeNamed conn updateUserQuery params
  where
    params =
      [ ":id" := userId user
      , ":username" := T.strip (username user)
      , ":shell" := T.strip (shell user)
      , ":homeDirectory" := T.strip (homeDirectory user)
      , ":realName" := T.strip (realName user)
      , ":phone" := T.strip (phone user)
      ]
