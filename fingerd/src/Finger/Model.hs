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
formatUser (User _ username shell homeDir realName _) =
  let
    login = BS.concat ["Login: ", encodeUtf8 username]
    dir = BS.concat ["Directory: ", encodeUtf8 homeDir]
    (spaceAfterLogin, spaceAfterDir) = spacesAfter 40 8 (BS.length login) (BS.length dir)
  in
    BS.concat
    [ login, tabsWithPadding spaceAfterLogin
    , "Name: ", encodeUtf8 realName, "\n"
    , dir, tabsWithPadding spaceAfterDir
    , "Shell: ", encodeUtf8 shell, "\n"
    ]
