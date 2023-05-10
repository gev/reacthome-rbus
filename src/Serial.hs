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
import           Foreign
import           System.Posix.IO
import           System.Posix.Terminal
import           System.Posix.Types



newtype SerialPort = SerialPort {fd :: Fd}

-- |Open and configure a serial port
openSerial :: FilePath -> IO SerialPort
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
recv :: SerialPort -> Ptr Word8 -> Int -> IO Int
recv port buf n = do
  result <- try $ fdReadBuf (fd port) buf (fromIntegral n) :: IO (Either IOError ByteCount)
  pure $ case result of
    Right n -> fromIntegral n
    Left  _ -> 0

-- |Send bytes
send :: SerialPort -> Ptr Word8 -> Int -> IO Int
send port msg n =
  fromIntegral <$> fdWriteBuf (fd port) msg (fromIntegral n)


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


withSerial :: FilePath -> (SerialPort -> IO a) -> IO a
withSerial dev = bracket (openSerial dev) closeSerial
