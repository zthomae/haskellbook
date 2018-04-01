{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (getLine, putStrLn)

import Control.Monad.Except (runExceptT)
import Data.Monoid ((<>))
import Data.Text.IO (getLine, putStrLn)
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Finger.Model (UserRow)
import Finger.Service (addUser)

-- TODO: Data validation would be nice here
readUserInfo :: IO UserRow
readUserInfo = do
  putStrLn "What is the new user's username?"
  username <- getLine
  putStrLn "What is the new user's shell?"
  shell <- getLine
  putStrLn "What is the new user's home directory?"
  home <- getLine
  putStrLn "What is the new user's full name?"
  fullName <- getLine
  putStrLn "What is the new user's phone number?"
  phone <- getLine
  return (Null, username, shell, home, fullName, phone)

main :: IO ()
main = do
  conn <- open "finger.db"
  userInfo <- readUserInfo
  -- TODO: This error handling is dopey
  result <- runExceptT $ addUser userInfo conn
  case result of
    Left err -> do
      putStrLn $ "Error: " <> SQLite.sqlErrorDetails err
      putStrLn $ "Context: " <> SQLite.sqlErrorContext err
    Right _ -> return ()
  SQLite.close conn
