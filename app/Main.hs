module Main where

import           Control.Monad
import           Data.ByteString.Char8
import           Serial


main :: IO ()
main = withSerial "/dev/ttyAMA0" $ \port -> forever $ do
        bytes <- recv port 512
        when (bytes /= empty) $
            print =<< recv port 512
