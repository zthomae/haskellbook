module Main where

import Database.SQLite.Simple (close, open)

import Finger.Service (createDatabase)

main :: IO ()
main = do
  conn <- open "finger.db"
  createDatabase conn
  close conn
