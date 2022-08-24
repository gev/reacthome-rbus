module Main where

import           System.RaspberryPi.GPIO
-- import           Control.Concurrent.Async (concurrently_)
-- import           Rbus                     (runRbus)

main :: IO ()
main = do
  -- res <- try $ do 
    setPinFunction Pin12 Output
    setPinFunction Pin16 Output
  -- case res of
  --   Left e -> print e
  --   _ -> print "Ok"
-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
