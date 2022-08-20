module Main where

import           Control.Concurrent.Async
import           Control.Monad            (forever)
import           Data.ByteString          (ByteString)
import           Data.ByteString.Builder
-- import           Network.Socke t.ByteString
import           Rbus
import           Serial
import           Serial.Types
-- import           Socket

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
main =
  concurrently_
    (withRbus "/dev/ttyAMA1")
    (withRbus "/dev/ttyAMA2")
