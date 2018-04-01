module Finger.Network where

import Data.ByteString (ByteString)
import Data.Int (Int16)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

bindSocket :: Int16 -> IO Socket
bindSocket port = do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                           Nothing
                           (Just (show port))
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bind sock (addrAddress serveraddr)
  listen sock 1
  return $ sock

receive :: Socket -> IO ByteString
receive soc = recv soc 1024

respondWith :: Maybe ByteString -> Socket -> IO ()
respondWith Nothing _ = return ()
respondWith (Just result) soc = sendAll soc result
