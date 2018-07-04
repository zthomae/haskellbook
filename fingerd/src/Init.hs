module Main where

import Database.SQLite.Simple (Connection, close, execute_, open)

import Finger.Queries (createUsers)

createDatabase :: Connection -> IO ()
createDatabase conn = execute_ conn createUsers

main :: IO ()
main = do
  conn <- open "finger.db"
  createDatabase conn
  close conn
