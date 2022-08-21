{-# LANGUAGE ForeignFunctionInterface #-}

-- |Library for controlling the GPIO pins on a Raspberry Pi (or any system using the Broadcom 2835 SOC). It is constructed
-- as a FFI wrapper over the BCM2835 library by Mike McCauley.
module System.RaspberryPi.GPIO (
    -- *Data types
    Pin(..),
    PinMode(..),
    LogicLevel,
    -- *General functions
    withGPIO,
    -- *GPIO specific functions
    setPinFunction,
    readPin,
    writePin,
    ) where

import           Control.Exception
import qualified Data.ByteString   as BS
import           Data.Maybe
import           Data.Tuple
import           Foreign
import           Foreign.C
import           Foreign.C.String
import           GHC.IO.Exception
-- |This describes the pins on the Raspberry Pi boards. Since the BCM2835 SOC internally uses different numbers (and these numbers
-- differ between versions, the library internally translates this pin number to the correct number.
data Pin =  -- |Pins for the P1 connector of the V2 revision of the Raspberry Pi
            Pin03|Pin05|Pin07|Pin08|Pin10|Pin11|Pin12|Pin13|Pin15|Pin16|Pin18|Pin19|Pin21|Pin22|Pin23|Pin24|Pin26|Pin36|
            -- |Pins for the P5 connector of the V2 revision of the Raspberry Pi
            PinP5_03|PinP5_04|PinP5_05|PinP5_06|
            -- |Pins for the P1 connector of the V1 revision of the Raspberry Pi
            PinV1_03|PinV1_05|PinV1_07|PinV1_08|PinV1_10|PinV1_11|PinV1_12|PinV1_13|PinV1_15|PinV1_16|PinV1_18|PinV1_19|PinV1_21|
            PinV1_22|PinV1_23|PinV1_24|PinV1_26
            deriving (Eq,Show)

-- |A GPIO pin can be either set to input mode, output mode or an alternative mode.
data PinMode = Input | Output | Alt0 | Alt1 | Alt2 | Alt3 | Alt4 | Alt5 deriving (Eq,Show)

instance Enum PinMode where -- bit strange, but just deriving Enum doesn't work because the numbers don't monotonically ascend
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table = [(Input, 0), (Output, 1), (Alt0, 4), (Alt1, 5), (Alt2, 6), (Alt3, 7), (Alt4, 3), (Alt5, 2)]

-- |Either high or low.
type LogicLevel = Bool

--initialises /dev/mem and obtaining the proper pointers to device registers. Don't use any other functions if this fails!
foreign import ccall unsafe "bcm2835.h bcm2835_init" initBCM2835              :: IO Int
--deallocates any memory, closes /dev/mem and stops the library in general
foreign import ccall unsafe "bcm2835.h bcm2835_close" stopBCM2835             :: IO Int
--sets debug level
foreign import ccall unsafe "bcm2835.h bcm2835_set_debug" setDebugBCM2835     :: CUChar -> IO ()

-- setFunction (input/output)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_fsel" c_setPinFunction    :: CUChar -> CUChar -> IO ()
-- setPin (zet een outputpin hoog/laag)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_write" c_writePin         :: CUChar -> CUChar -> IO ()
-- readPin (geeft weer of een pin hoog/laag is)
foreign import ccall unsafe "bcm2835.h bcm2835_gpio_lev" c_readPin            :: CUChar -> IO CUChar


-- |Any IO computation that accesses the GPIO pins using this library should be wrapped with this function; ie @withGPIO $ do foo@.
-- It prepares the file descriptors to /dev/mem and makes sure everything is safely deallocated if an exception occurs. The behavior
-- when accessing the GPIO outside of this function is undefined.
withGPIO :: IO a -> IO a
withGPIO f = bracket
    initBCM2835
        (const stopBCM2835) --const because you don't care about the output of initBCM2835
        (\a -> if a == 0 then throwIO ioe else f) -- init returning 0 is not good
            where ioe = IOError Nothing IllegalOperation "GPIO: " "Unable to start GPIO." Nothing Nothing


-- Mapping raspberry pi pin number to internal bmc2835 pin number, ugly solution, but meh. Also, the existence of mutiple versions
-- of the pin layout makes this the most elegant solution without resorting to state monads (which I don't want to do because that
-- would hamper the simplicity of having writePin and readPin be simple IO actions). As this function isn't exported anyway, the
-- user should never be troubled by all this.
getHwPin :: Pin -> CUChar
--P1 connector on V1 boards
getHwPin PinV1_03 = 0
getHwPin PinV1_05 = 1
getHwPin PinV1_07 = 4
getHwPin PinV1_08 = 14
getHwPin PinV1_10 = 15
getHwPin PinV1_11 = 17
getHwPin PinV1_12 = 18
getHwPin PinV1_13 = 21
getHwPin PinV1_15 = 22
getHwPin PinV1_16 = 23
getHwPin PinV1_18 = 24
getHwPin PinV1_19 = 10
getHwPin PinV1_21 = 9
getHwPin PinV1_22 = 25
getHwPin PinV1_23 = 11
getHwPin PinV1_24 = 8
getHwPin PinV1_26 = 7
--P1 connector on V2 boards
getHwPin Pin03    = 2
getHwPin Pin05    = 3
getHwPin Pin07    = 4
getHwPin Pin08    = 14
getHwPin Pin10    = 15
getHwPin Pin11    = 17
getHwPin Pin12    = 18
getHwPin Pin13    = 27
getHwPin Pin15    = 22
getHwPin Pin16    = 23
getHwPin Pin18    = 24
getHwPin Pin19    = 10
getHwPin Pin21    = 9
getHwPin Pin22    = 25
getHwPin Pin23    = 11
getHwPin Pin24    = 8
getHwPin Pin26    = 7
getHwPin Pin36    = 16
--for the P5 connector on V2 boards
getHwPin PinP5_03 = 28
getHwPin PinP5_04 = 29
getHwPin PinP5_05 = 30
getHwPin PinP5_06 = 31

withPin :: (Num n, Enum a) => (CUChar -> n -> r) -> Pin -> a -> r
withPin f pin value = f (getHwPin pin) (fromIntegral $ fromEnum value)

-- |Sets the pin to either 'Input' or 'Output' mode.
setPinFunction :: Pin -> PinMode -> IO ()
setPinFunction = withPin c_setPinFunction

-- |Sets the specified pin to either 'True' or 'False'.
writePin :: Pin -> LogicLevel -> IO ()
writePin = withPin c_writePin

-- |Returns the current state of the specified pin.
readPin :: Pin -> IO LogicLevel
readPin pin = toEnum . fromIntegral <$> c_readPin (getHwPin pin)

