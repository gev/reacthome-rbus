module Serial
  ( SerialPort
  , openSerial
  , closeSerial
  , recv
  , send
  , flush
  , drain
  , withSerial
  ) where

import           Control.Exception
import           Data.ByteString.Char8
import           Data.Typeable
import           System.Posix.ByteString
import           System.Posix.IO.ByteString
import           System.Posix.Terminal
import           System.Posix.Types



newtype SerialPort = SerialPort {fd :: Fd}

-- |Open and configure a serial port
openSerial :: RawFilePath -> IO SerialPort
openSerial dev = do
  fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True
                                               , nonBlock = True
                                               }
  setFdOption fd' NonBlockingRead False
  let serial_port = SerialPort fd'
  setSerialSettings serial_port


setSerialSettings :: SerialPort -> IO SerialPort
setSerialSettings port = do
  termOpts <- getTerminalAttributes $ fd port
  let termOpts' = configureSettings termOpts
  setTerminalAttributes (fd port) termOpts' WhenDrained
  return $ SerialPort (fd port)

-- |Receive bytes, given the maximum number
recv :: SerialPort -> Int -> IO ByteString
recv port n = do
  result <- try $ fdRead (fd port) (fromIntegral n) :: IO (Either IOError ByteString)
  pure $ case result of
    Right str -> str
    Left  _   -> empty

-- |Send bytes
send :: SerialPort -> ByteString -> IO Int
send port msg =
  fromIntegral <$> fdWrite (fd port) msg


-- |Flush buffers
flush :: SerialPort -> IO ()
flush port = discardData (fd port) BothQueues

drain :: SerialPort -> IO ()
drain port = drainOutput (fd port)


-- |Close the serial port
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


withSerial :: RawFilePath -> (SerialPort -> IO a) -> IO a
withSerial dev = bracket (openSerial dev) closeSerial
