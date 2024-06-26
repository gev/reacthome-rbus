module Main where

import           Control.Monad
import           Serial


main :: IO ()
main = withSerial "/dev/ttyAMA0" $ \port -> do
    forever $ do
        buff <- recv port 512
        print buff
