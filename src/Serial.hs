module Serial where

import           Control.Exception
import           Data.ByteString
import           Data.Either
import           Foreign
import           GHC.IO.Encoding
import           System.IO
import           System.Posix.IO
import qualified System.Posix.IO.ByteString as BIO
import           System.Posix.Terminal
import           System.Posix.Types


newtype SerialPort = SerialPort {fd :: Fd}

-- |Open and configure a serial port
openSerial
    :: FilePath         -- ^ Serial port, such as @\/dev\/ttyS0@ or @\/dev\/ttyUSB0@
    -> IO SerialPort
openSerial dev = do
    fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True, nonBlock = True}
    setFdOption fd' NonBlockingRead False
    termOpts <- getTerminalAttributes fd'
    setTerminalAttributes fd' (configureSettings termOpts) WhenDrained
    pure $ SerialPort fd'

-- |Receive bytes, given the maximum number
recv
    :: SerialPort
    -> Int
    -> IO ByteString
recv port n = BIO.fdRead (fd port) $ fromIntegral n

-- |Send bytes
send
    :: SerialPort
    -> ByteString
    -> IO Int          -- ^ Number of bytes actually sent
send port msg = fromIntegral <$> BIO.fdWrite (fd port) msg


flush :: SerialPort -> IO ()
flush port = discardData (fd port) BothQueues

drain :: SerialPort -> IO ()
drain port = drainOutput (fd port)

closeSerial :: SerialPort -> IO ()
closeSerial = closeFd . fd

configureSettings :: TerminalAttributes -> TerminalAttributes
configureSettings termOpts =
    termOpts `withInputSpeed`   1_000_000
             `withOutputSpeed`  1_000_000
             `withBits`         8
             `withoutMode`      TwoStopBits
             `withoutMode`      EnableParity
             `withoutMode`      StartStopInput
             `withoutMode`      StartStopOutput
             `withoutMode`      EnableEcho
             `withoutMode`      EchoErase
             `withoutMode`      EchoKill
             `withoutMode`      ProcessInput
             `withoutMode`      ProcessOutput
             `withoutMode`      MapCRtoLF
             `withoutMode`      EchoLF
             `withoutMode`      HangupOnClose
             `withoutMode`      KeyboardInterrupts
             `withoutMode`      ExtendedFunctions
             `withMode`         LocalMode
             `withMode`         ReadEnable
             `withTime`         0
             `withMinInput`     1

withSerial :: FilePath -> (SerialPort -> IO a) -> IO a
withSerial dev = bracket (openSerial dev) closeSerial
