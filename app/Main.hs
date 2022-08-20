module Main where

import           Control.Concurrent
import           Control.Monad           (forever, void)
-- import           Network.Socket.ByteString
import           Data.ByteString.Builder
import           Rbus
import           Serial
import           Serial.Types
-- import           Socket

-- main :: IO ()
-- main = do
--   sock <- createSocket "127.0.0.1"
--   void $ forkIO $ forever $ do
--     result <- recvFrom sock 4096
--     forkIO $ worker sock result

prettyShow = show . toLazyByteString . byteStringHex

withRbus :: FilePath -> IO ()
withRbus device = withSerial device rbusSerialSettings
  $ \port -> void $ forkIO $ forever $ do
    res <- recv port 1024
    print $ device <> ": " <> prettyShow res


main :: IO ()
main = do
  withRbus "/dev/ttyAMA1"
  withRbus "/dev/ttyAMA2"
