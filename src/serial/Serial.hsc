{-# LANGUAGE ForeignFunctionInterface #-}
module Serial 
  ( SerialPort
  , openSerial
  , closeSerial
  , recv
  , send
  , flush
  , setSerialSettings
  , withSerial
  ) where 
 
import Data.Word

import GHC.IO.Encoding

import System.Posix.IO 
import System.Posix.Types (Fd, ByteCount, CSpeed(..))
import System.Posix.Terminal

import Foreign (Ptr, nullPtr, castPtr, alloca, peek, with)
import Foreign.C

import Data.Typeable
import Data.Bits

import qualified Data.ByteString.Char8 as B
import qualified Control.Exception     as Ex

import Serial.Types


data SerialPort = SerialPort
  { fd           :: Fd
  , portSettings :: SerialPortSettings
  } deriving (Show, Typeable)

-- |Open and configure a serial port
openSerial :: FilePath            -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
           -> SerialPortSettings
           -> IO SerialPort
openSerial dev settings = do
  fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True, nonBlock = True }
  setTIOCEXCL fd'
  setFdOption fd' NonBlockingRead False
  let serial_port = SerialPort fd' defaultSerialSettings
  setSerialSettings serial_port settings

-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO B.ByteString
recv port n = do
  result <- Ex.try $ fdRead (fd port) count :: IO (Either IOError (String, ByteCount))
  return $ case result of
    Right (str, _) -> B.pack str
    Left _         -> B.empty
  where
    count = fromIntegral n


-- |Send bytes
send
  :: SerialPort
  -> B.ByteString
  -> IO Int          -- ^ Number of bytes actually sent
send port msg =
  fromIntegral <$> fdWrite (fd port) (B.unpack msg)


-- |Flush buffers
flush :: SerialPort -> IO ()
flush port = discardData (fd port) BothQueues


-- |Close the serial port
closeSerial :: SerialPort -> IO ()
closeSerial = closeFd . fd


#include <sys/ioctl.h>

foreign import ccall "ioctl" c_ioctl :: CInt -> CInt -> Ptr () -> IO CInt

cIoctl' :: Fd -> Int -> Ptr d -> IO ()
cIoctl' f req =
  throwErrnoIfMinus1_ "ioctl" .
     c_ioctl (fromIntegral f) (fromIntegral req) . castPtr


setTIOCEXCL :: Fd -> IO ()
setTIOCEXCL fd' = cIoctl' fd' #{const TIOCEXCL} nullPtr

-- |Set the serial port settings
setSerialSettings :: SerialPort           -- ^ The currently opened serial port
                  -> SerialPortSettings   -- ^ The new settings
                  -> IO SerialPort        -- ^ New serial port
setSerialSettings port new_settings = do
  termOpts <- getTerminalAttributes $ fd port
  let termOpts' = configureSettings termOpts new_settings
  setTerminalAttributes (fd port) termOpts' Immediately
  return $ SerialPort (fd port) new_settings

-- |Get the serial port settings
getSerialSettings :: SerialPort -> SerialPortSettings
getSerialSettings = portSettings

withParity :: TerminalAttributes -> Parity -> TerminalAttributes
withParity termOpts Even =
    termOpts `withMode` EnableParity
             `withoutMode` OddParity
withParity termOpts Odd =
    termOpts `withMode` EnableParity
             `withMode` OddParity
withParity termOpts NoParity =
    termOpts `withoutMode` EnableParity

withStopBits :: TerminalAttributes -> StopBits -> TerminalAttributes
withStopBits termOpts One =
    termOpts `withoutMode` TwoStopBits
withStopBits termOpts Two =
    termOpts `withMode` TwoStopBits

configureSettings :: TerminalAttributes -> SerialPortSettings -> TerminalAttributes
configureSettings termOpts settings =
    termOpts `withInputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withOutputSpeed` commSpeedToBaudRate (commSpeed settings)
             `withBits` fromIntegral (bitsPerWord settings)
             `withStopBits` stopb settings
             `withParity` parity settings
             `withoutMode` StartStopInput
             `withoutMode` StartStopOutput
             `withoutMode` EnableEcho
             `withoutMode` EchoErase
             `withoutMode` EchoKill
             `withoutMode` ProcessInput
             `withoutMode` ProcessOutput
             `withoutMode` MapCRtoLF
             `withoutMode` EchoLF
             `withoutMode` HangupOnClose
             `withoutMode` KeyboardInterrupts
             `withoutMode` ExtendedFunctions
             `withMode` LocalMode
             `withMode` ReadEnable
             `withTime` 0
             `withMinInput` 1

commSpeedToBaudRate :: Word32 -> BaudRate
commSpeedToBaudRate = BaudRate . CSpeed . fromIntegral

-- |Use the specified serial port
withSerial :: FilePath -> SerialPortSettings -> ( SerialPort -> IO a ) -> IO a
withSerial dev settings = Ex.bracket (openSerial dev settings) closeSerial
