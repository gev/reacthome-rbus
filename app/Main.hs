module Main where

import           System.RaspberryPi.GPIO
-- import           Control.Concurrent.Async (concurrently_)
-- import           Rbus                     (runRbus)

main :: IO ()
main = do
  setPinFunction Pin10 Output
  setPinFunction Pin19 Output
-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
