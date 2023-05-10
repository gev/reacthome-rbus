module Main where

import           Control.Monad
import           Foreign
import           Serial


main :: IO ()
main = withSerial "/dev/ttyAMA0" $ \port -> do
    buf <- mallocArray 512
    forever $ do
        n <- recv port buf 512
        when (n > 0) $
            print =<< peekArray n buf
    free buf
