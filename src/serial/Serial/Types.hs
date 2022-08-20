module Serial.Types where

import           Data.Word

data StopBits = One | Two
  deriving (Show, Read, Eq, Bounded)

data Parity = Even | Odd | NoParity
  deriving (Show, Read, Eq)

data SerialPortSettings = SerialPortSettings
  { commSpeed   :: Word32,      -- ^ baudrate
    bitsPerWord :: Word8,       -- ^ Number of bits in a word
    stopb       :: StopBits,    -- ^ Number of stop bits
    parity      :: Parity,      -- ^ Type of parity
    timeout     :: Int          -- ^ Timeout when receiving a char in microseconds
  } deriving (Show, Read, Eq)


-- | Most commonly used configuration
--  - 9600 baud
--  - 8 data bits
--  - 1 stop bit
--  - no parity
--  - 1 tenth of a second timeout
--
defaultSerialSettings :: SerialPortSettings
defaultSerialSettings =
  SerialPortSettings 9600 8 One NoParity 1
