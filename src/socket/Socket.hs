module Socket where

import           Control.Concurrent
import           Control.Exception         (bracketOnError)
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Network.Socket            hiding (recvFrom, sendTo)
import           Network.Socket.ByteString

hints :: AddrInfo
hints = defaultHints
  { addrFamily      = AF_INET
  , addrSocketType  = Datagram
  }

service :: ServiceName
service = "2016"

getAddress :: HostName -> IO AddrInfo
getAddress host =
  head <$> getAddrInfo (Just hints) (Just host) (Just service)

createSocket :: HostName -> IO Socket
createSocket host = do
  addr <- getAddress host
  bracketOnError (openSocket addr) close
    $ \sock -> do
      bind sock $ addrAddress addr
      pure sock

worker :: Socket -> (ByteString, SockAddr) -> IO ()
worker sock (msg, client) = do
  print client
  print msg
  -- void $ sendTo sock msg client


