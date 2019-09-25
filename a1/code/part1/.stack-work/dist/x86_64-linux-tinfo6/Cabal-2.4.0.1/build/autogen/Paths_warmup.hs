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

bindir     = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/bin"
libdir     = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/lib/x86_64-linux-ghc-8.6.5/warmup-0.0.0-E3rkwZ1qSV6F70KyAAh2Ko"
dynlibdir  = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/share/x86_64-linux-ghc-8.6.5/warmup-0.0.0"
libexecdir = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/libexec/x86_64-linux-ghc-8.6.5/warmup-0.0.0"
sysconfdir = "/home/bcvb/Documents/school/datalogi-cs/AP/AP-git/a1/code/part1/.stack-work/install/x86_64-linux-tinfo6/40f61f09955389d4b322b79cb530a7404a60a03514da470f38dcc943da8af146/8.6.5/etc"

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
