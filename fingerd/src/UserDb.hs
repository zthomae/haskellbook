{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Prelude hiding (getLine, putStr, putStrLn)

import Control.Exception (try)
import Control.Monad (void)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Foldable (mapM_)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (getLine, putStr, putStrLn)
import Data.Text.Read (decimal)
import Data.Vector (Vector, (!?))
import qualified Data.Vector as V
import Database.SQLite.Simple hiding (bind, close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Finger.Display (formatUser)
import Finger.Model (User(..), UserRow)
import Finger.Queries (getUsers, insertUser, updateUser)

data UserChoice = CreateUser | ModifyUser

describeUserChoice :: UserChoice -> Text
describeUserChoice CreateUser = "Create a new user"
describeUserChoice ModifyUser = "Modify an exising user"

printSelections :: [Text] -> IO ()
printSelections entries =
  let
    entriesWithIndices = zip entries [1..]
    renderRow = \(entry, index) -> T.concat
      [ "\t"
      , T.pack (show index)
      , ". "
      , entry
      ]
    in
      mapM_ (putStrLn . renderRow) entriesWithIndices

lookupSelection :: Vector a -> Text -> Maybe a
lookupSelection entries input = do
  num <- case decimal input of
    Right (n, _) -> Just n
    Left _ -> Nothing
  entries !? (num - 1)

data UserFieldChoice = UserName | Shell | HomeDir | RealName | PhoneNumber | Finish

describeUserFieldChoice :: UserFieldChoice -> Text
describeUserFieldChoice UserName = "Set new username"
describeUserFieldChoice Shell = "Set new shell"
describeUserFieldChoice HomeDir = "Set new home directory"
describeUserFieldChoice RealName = "Set new full name"
describeUserFieldChoice PhoneNumber = "Set new phone number"
describeUserFieldChoice Finish = "Finish and save"

readUserChoice :: IO UserChoice
readUserChoice =
  let
    choices = V.fromList [CreateUser, ModifyUser]
    descriptions = V.toList (fmap describeUserChoice choices)
    inner = do
      putStrLn "Please make a selection:"
      printSelections descriptions
      input <- getLine
      let selection = lookupSelection choices input
      case selection of
        Just a -> return a
        Nothing -> putStrLn "Unknown selection" >> inner
  in
    inner

-- Note: this relies on the fact that every column stores Text
-- Other note: A lens might be nice here
modifyUser :: UserFieldChoice -> Text -> User -> User
modifyUser field input user =
  case field of
    UserName -> user { username = input }
    Shell -> user { shell = input }
    HomeDir -> user { homeDirectory = input }
    RealName -> user { realName = input }
    PhoneNumber -> user { phone = input }
    Finish -> user

data FinishAction = Cancel | Save

describeFinishAction :: FinishAction -> Text
describeFinishAction Cancel = "Cancel and exit"
describeFinishAction Save = "Save"

editUser :: User -> IO (Maybe User)
editUser user =
  let
    choices = V.fromList [UserName, Shell, HomeDir, RealName, PhoneNumber, Finish]
    descriptions = V.toList $ fmap describeUserFieldChoice choices
    exits = V.fromList [Save, Cancel]
    exitDescriptions = V.toList $ fmap describeFinishAction exits
    finish user = do
      putStrLn "The following information will be saved:\n"
      putStrLn (formatUser user)
      putStrLn "Are you sure?"
      printSelections exitDescriptions
      input <- getLine
      let selection = lookupSelection exits input
      case selection of
        Just Cancel -> return Nothing
        Just Save -> return $ Just user
        Nothing -> putStrLn "Unknown selection" >> finish user
    inner user = do
      putStrLn "Select a field to edit:"
      printSelections descriptions
      input <- getLine
      let selection = lookupSelection choices input
      case selection of
        Just Finish -> finish user
        Just f -> do
          putStrLn (T.concat ["Please enter a new ", describeUserFieldChoice f, ":"])
          input <- getLine
          inner (modifyUser f input user)
        Nothing -> putStrLn "Unknown selection" >> inner user
    in
      inner user

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

addUser :: UserRow -> Connection -> ExceptT SQLError IO ()
addUser user dbConn = ExceptT (try $ SQLite.execute dbConn insertUser user)

main :: IO ()
main = do
  conn <- open "finger.db"
  selection <- readUserChoice
  case selection of
    CreateUser -> do
      userInfo <- readUserInfo
      -- TODO: This error handling is dopey
      result <- runExceptT $ addUser userInfo conn
      case result of
        Left err -> do
          putStrLn $ "Error: " <> SQLite.sqlErrorDetails err
          putStrLn $ "Context: " <> SQLite.sqlErrorContext err
        Right _ -> return ()
    ModifyUser -> do
      users <- V.fromList <$> getUsers conn
      let
        descriptions = V.toList (fmap username users)
        inner = do
          putStrLn "Please select a user:"
          printSelections descriptions
          input <- getLine
          let userSelection = lookupSelection users input
          case userSelection of
            Just user -> do
              putStrLn "\nFound a user:"
              putStr (formatUser user)
              updatedUser <- editUser user
              case updatedUser of
                Just u -> putStrLn "Saving..." >> updateUser u conn
                Nothing -> putStrLn "Exiting"
            Nothing -> putStrLn "Unknown user" >> inner
      void $ inner
  SQLite.close conn
