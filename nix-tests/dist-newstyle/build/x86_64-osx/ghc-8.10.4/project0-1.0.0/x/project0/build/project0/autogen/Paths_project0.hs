{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_project0 (
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
version = Version [1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/japiirainen/.cabal/bin"
libdir     = "/Users/japiirainen/.cabal/lib/x86_64-osx-ghc-8.10.4/project0-1.0.0-inplace-project0"
dynlibdir  = "/Users/japiirainen/.cabal/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/japiirainen/.cabal/share/x86_64-osx-ghc-8.10.4/project0-1.0.0"
libexecdir = "/Users/japiirainen/.cabal/libexec/x86_64-osx-ghc-8.10.4/project0-1.0.0"
sysconfdir = "/Users/japiirainen/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project0_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project0_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "project0_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "project0_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project0_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project0_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
