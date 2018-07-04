{-# LANGUAGE RecordWildCards #-}

module Finger.Model where

import Data.Text (Text)
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
