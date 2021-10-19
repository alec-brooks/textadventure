{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_textadventure (
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/ack/.cabal/bin"
libdir     = "/home/ack/.cabal/lib/x86_64-linux-ghc-9.0.1/textadventure-0.1.0.0-inplace-textadventure"
dynlibdir  = "/home/ack/.cabal/lib/x86_64-linux-ghc-9.0.1"
datadir    = "/home/ack/.cabal/share/x86_64-linux-ghc-9.0.1/textadventure-0.1.0.0"
libexecdir = "/home/ack/.cabal/libexec/x86_64-linux-ghc-9.0.1/textadventure-0.1.0.0"
sysconfdir = "/home/ack/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "textadventure_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "textadventure_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "textadventure_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "textadventure_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "textadventure_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "textadventure_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
