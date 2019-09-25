{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_warmup (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
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
version = Version [0,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/bin"
libdir     = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/lib/x86_64-linux-ghc-8.6.5/warmup-0.0.0-4AovEgY7veW1fuxhu9C1fs"
dynlibdir  = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/share/x86_64-linux-ghc-8.6.5/warmup-0.0.0"
libexecdir = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/libexec/x86_64-linux-ghc-8.6.5/warmup-0.0.0"
sysconfdir = "/home/kalel/Documents/Skole/AP/ap_git/a2/code/part1/.stack-work/install/x86_64-linux/89a640d7248b9777e369f86686f70dbfd62798f6d7cd69572373b59faaed44da/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "warmup_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "warmup_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "warmup_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "warmup_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "warmup_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "warmup_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
