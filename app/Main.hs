module Main where

import           System.RaspberryPi.GPIO
-- import           Control.Concurrent.Async (concurrently_)
import           Control.Concurrent      (threadDelay)
import           Control.Monad           (forever)
import qualified Data.ByteString.Char8   as B
import           Rbus                    (runRbus)
import           Serial                  (send, withSerial)
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
main =
  withSerial "/dev/ttyAMA1" (SerialPortSettings 9600 8 One NoParity 0)
    $ \port -> forever $ do
      withGPIO $ do
        on GPIO16
        send port $ B.pack "Hello world!"
        off GPIO16
        threadDelay 20_000_000

-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
