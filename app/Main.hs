module Main where

import           Control.Monad
import           Serial


main :: IO ()
main = withSerial "/dev/ttyAMA0" $ \port -> forever $
        print =<< recv port 512
