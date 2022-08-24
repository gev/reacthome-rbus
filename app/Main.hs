module Main where

import           System.RaspberryPi.GPIO
import           System.RaspberryPi.GPIO (writePin)
-- import           Control.Concurrent.Async (concurrently_)
-- import           Rbus                     (runRbus)

toggle gpio = do
  v <- readPin gpio
  print $ show gpio <> ": " <> show v
  writePin gpio $ not v

main :: IO ()
main = withGPIO $ do
  toggle GPIO12
  toggle GPIO16

-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
