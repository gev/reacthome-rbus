module Main where

import           System.RaspberryPi.GPIO
-- import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forever)
import qualified Data.ByteString.Char8   as B
import           Rbus                    (runRbus)
import           Serial                  (drain, flush, send, withSerial)
import           Serial.Types            (Parity (NoParity),
                                          SerialPortSettings (SerialPortSettings),
                                          StopBits (One))
import           System.Posix            (sleep)


toggle :: Pin -> IO ()
toggle gpio = do
  v <- readPin gpio
  print $ show gpio <> ": " <> show v
  writePin gpio $ not v

write :: LogicLevel -> Pin -> IO ()
write = flip writePin

on :: Pin -> IO ()
on = write True

off :: Pin -> IO ()
off = write False

main :: IO ()
main = withGPIO . withSerial
      "/dev/ttyAMA1" (SerialPortSettings 9600 8 One NoParity 0)
      $ \port -> forever $ do
        on GPIO16
        send port $ B.pack "Hello world!"
        -- drain port
        flush port
        off GPIO16
        threadDelay 1_000_000

-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
