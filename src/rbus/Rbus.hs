module Rbus where

import           Control.Monad           (forever)
import           Data.ByteString         as B
import           Data.ByteString.Builder (byteStringHex, toLazyByteString)
import           Serial                  (recv, withSerial)
import           Serial.Types

rbusSerialSettings :: SerialPortSettings
rbusSerialSettings =
  SerialPortSettings 1_000_000 8 One NoParity 0


prettyShow :: ByteString -> String
prettyShow = show . toLazyByteString . byteStringHex

runRbus :: FilePath -> IO ()
runRbus device = withSerial device rbusSerialSettings
  $ \port -> forever $ do
    res <- recv port 1
    if B.length res > 0
      then print $ device <> ": " <> prettyShow res
      else pure ()
