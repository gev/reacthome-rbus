module Serial where

import           Control.Exception
import           Foreign
import           System.Posix.IO
import           System.Posix.Terminal
import           System.Posix.Types


newtype SerialPort = SerialPort {fd :: Fd}

openSerial :: FilePath -> IO SerialPort
openSerial dev = do
  fd' <- openFd dev ReadWrite defaultFileFlags { noctty = True, nonBlock = True}
  setFdOption fd' NonBlockingRead False
  termOpts <- getTerminalAttributes fd'
  setTerminalAttributes fd' (configureSettings termOpts) WhenDrained
  pure $ SerialPort fd'

recv :: SerialPort -> Ptr Word8 -> Int -> IO Int
recv port buf n = fromIntegral <$> fdReadBuf (fd port) buf (fromIntegral n)

send :: SerialPort -> Ptr Word8 -> Int -> IO Int
send port msg n = fromIntegral <$> fdWriteBuf (fd port) msg (fromIntegral n)

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
