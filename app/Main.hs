module Main where

import           Control.Concurrent.Async (concurrently_)
import           Rbus                     (runRbus)

main :: IO ()
main =
  concurrently_
    (runRbus "/dev/ttyAMA1")
    (runRbus "/dev/ttyAMA2")
