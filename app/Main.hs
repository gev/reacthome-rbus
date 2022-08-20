module Main where

import           Control.Concurrent
import           Control.Monad           (forever, void)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Builder
-- import           Network.Socke t.ByteString
import           Rbus
import           Serial
import           Serial.Types
-- import           Socket
import           System.Posix            (sleep)

-- main :: IO ()
-- main = do
--   sock <- createSocket "127.0.0.1"
--   forever $ do
--     result <- recvFrom sock 4096
--     forkIO $ worker sock result

prettyShow :: ByteString -> String
prettyShow = show . toLazyByteString . byteStringHex

withRbus :: FilePath -> IO ()
withRbus device = withSerial device rbusSerialSettings
  $ \port -> forever $ do
    res <- recv port 1024
    print $ device <> ": " <> prettyShow res


main :: IO ()
main =  do
  void $ forkIO $ withRbus "/dev/ttyAMA1"
  withRbus "/dev/ttyAMA2"
