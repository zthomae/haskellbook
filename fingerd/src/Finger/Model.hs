{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Finger.Model where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Database.SQLite.Simple
import Database.SQLite.Simple.Types (Null)

type UserRow = (Null, Text, Text, Text, Text, Text)

data User =
  User {
    userId :: Integer
  , username :: Text
  , shell :: Text
  , homeDirectory :: Text
  , realName :: Text
  , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field
                  <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  ["Login: ", encodeUtf8 username, "\t\t\t\t",
   "Name: ", encodeUtf8 realName, "\n",
   "Directory: ", encodeUtf8 homeDir, "\t\t\t\t",
   "Shell: ", encodeUtf8 shell, "\n"]
