module Rbus where

import           Control.Monad           (forever, when)
import           Data.ByteString         as B
import           Data.ByteString.Builder (byteStringHex, toLazyByteString)
import           Serial


prettyShow :: ByteString -> String
prettyShow = show . byteStringHex
-- prettyShow = show . toLazyByteString . byteStringHex

runRbus :: FilePath -> IO ()
runRbus device =
  withSerial "/dev/ttyAMA0"
  $ \port -> forever $ do
    res <- recv port 1
    let resLenght = B.length res
    when (resLenght > 0) $
      print $ device <> ": " <> show  resLenght <> " " <> prettyShow res
