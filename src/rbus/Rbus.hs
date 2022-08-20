module Rbus where

import           Serial.Types

rbusSerialSettings :: SerialPortSettings
rbusSerialSettings =
  SerialPortSettings 1_000_000 8 One NoParity 1


