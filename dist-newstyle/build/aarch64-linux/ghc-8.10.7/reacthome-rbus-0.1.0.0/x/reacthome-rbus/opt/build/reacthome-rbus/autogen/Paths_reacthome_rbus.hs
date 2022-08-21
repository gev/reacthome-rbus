{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_reacthome_rbus (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude


#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/pi/.cabal/bin"
libdir     = "/home/pi/.cabal/lib/aarch64-linux-ghc-8.10.7/reacthome-rbus-0.1.0.0-inplace-reacthome-rbus"
dynlibdir  = "/home/pi/.cabal/lib/aarch64-linux-ghc-8.10.7"
datadir    = "/home/pi/.cabal/share/aarch64-linux-ghc-8.10.7/reacthome-rbus-0.1.0.0"
libexecdir = "/home/pi/.cabal/libexec/aarch64-linux-ghc-8.10.7/reacthome-rbus-0.1.0.0"
sysconfdir = "/home/pi/.cabal/etc"

getBinDir     = catchIO (getEnv "reacthome_rbus_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "reacthome_rbus_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "reacthome_rbus_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "reacthome_rbus_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "reacthome_rbus_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "reacthome_rbus_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
