module Main where

import           System.RaspberryPi.GPIO
import           System.RaspberryPi.GPIO (writePin)
-- import           Control.Concurrent.Async (concurrently_)
-- import           Rbus                     (runRbus)

main :: IO ()
main = withGPIO $ do
  gpio12 <- readPin Pin12
  print $ "gpio12: " <> show gpio12
  gpio16 <- readPin Pin16
  print $ "gpio16: " <> show gpio16
  writePin Pin12 True
  writePin Pin16 True
  -- res <- try $ do
    -- setPinFunction Pin12 Output
    -- setPinFunction Pin16 Output
  -- case res of
  --   Left e -> print e
  --   _ -> print "Ok"
-- main =
--   concurrently_
--     (runRbus "/dev/ttyAMA1")
--     (runRbus "/dev/ttyAMA2")
