module Main where

-- import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent    (threadDelay)
import           Control.Monad         (forever)
import qualified Data.ByteString.Char8 as B
import           Rbus                  (runRbus)
import           System.Posix          (sleep)


main :: IO ()
main = undefined
  -- withGPIO . withSerial
  --     "/dev/ttyAMA1" (SerialPortSettings 9600 8 One NoParity 0)
  --     $ \port -> forever $ do
  --       on GPIO16
  --       send port $ B.pack "Hello world!"
  --       drain port
  --       -- flush port
  --       off GPIO16
  --       threadDelay 1_000_000

-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
