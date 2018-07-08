{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (try)
import Control.Monad (forever)
import Control.Monad.Except (ExceptT(..), runExceptT)
import qualified Data.ByteString as BS
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Vector ((!?))
import qualified Data.Vector as V
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Database.SQLite.Simple hiding (bind, close)
import Database.SQLite.Simple.Types
import qualified Database.SQLite.Simple as SQLite
import Network.Socket hiding (bindSocket)
import System.Posix.Signals (Handler, Handler(CatchOnce), installHandler, sigINT, sigTERM)

import Finger.Display (formatUser)
import Finger.Model (User(..), UserRow)
import Finger.Network (bindSocket, receive, respondWith)
import Finger.Queries (getUser, getUsers, insertUser)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- receive soc
  resp <- case msg of
    "\r\n" -> do
      rows <- getUsers dbConn
      let usernames = map username rows
          newlineSeparated = T.concat $ intersperse "\n" usernames
      return $ Just (encodeUtf8 newlineSeparated)
    name -> (getUser (decodeUtf8 name) dbConn) >>= \user -> return $ encodeUtf8 <$> formatUser <$> user
  respondWith resp soc

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  close soc

addUser :: UserRow -> Connection -> ExceptT SQLError IO ()
addUser user dbConn = ExceptT (try $ SQLite.execute dbConn insertUser user)

userMaintenance :: Connection -> Socket -> IO ()
userMaintenance dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got maintenance connection, handling query"
  msg <- receive soc
  let parts = V.fromList $ BS.split (toEnum $ fromEnum '\t') msg
  resp <-
    if V.null parts
    then return $ Just (encodeUtf8 "Invalid request")
    else
      let
        optionalField idx = T.strip $ decodeUtf8 $ fromMaybe BS.empty (parts !? idx)
        _shell = optionalField 1
        _homeDirectory = optionalField 2
        _realName = optionalField 3
        _phone = optionalField 4
      in case T.strip . decodeUtf8 <$> parts !? 0 of
        Nothing -> return $ Just "Username must be provided"
        Just _username -> do
          result <- runExceptT $ addUser (Null, _username, _shell, _homeDirectory, _realName, _phone) dbConn
          case result of
            Left err -> do
              TIO.putStrLn $ "Error: " <> SQLite.sqlErrorDetails err
              TIO.putStrLn $ "Context: " <> SQLite.sqlErrorContext err
              return Nothing
            Right _ -> return $ Just (encodeUtf8 ("Created user " <> _username <> "\n"))
  respondWith resp soc
  close soc

handler :: MVar () -> Handler
handler var = CatchOnce $ putMVar var ()

main :: IO ()
main = withSocketsDo $ do
  sock <- bindSocket 79
  controlSock <- bindSocket 8000
  -- only one connection open at a time
  conn <- open "finger.db"
  var <- newEmptyMVar
  _ <- installHandler sigINT (handler var) Nothing
  _ <- installHandler sigTERM (handler var) Nothing
  _ <- forkIO (handleQueries conn sock)
  _ <- forkIO (userMaintenance conn controlSock)
  takeMVar var
  SQLite.close conn
  close controlSock
  close sock
