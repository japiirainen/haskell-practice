{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_validate (
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

bindir     = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/bin"
libdir     = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/lib/x86_64-osx-ghc-8.8.4/validate-0.1.0.0-BLGfquqd5LEIF6IqHEbZdK-validate"
dynlibdir  = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/lib/x86_64-osx-ghc-8.8.4"
datadir    = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/share/x86_64-osx-ghc-8.8.4/validate-0.1.0.0"
libexecdir = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/libexec/x86_64-osx-ghc-8.8.4/validate-0.1.0.0"
sysconfdir = "/Users/joona/dev/haskell-practice/validations/validate/.stack-work/install/x86_64-osx/91794f8331dd1222401ce72788d353dddc2f80045762f8846ec60d22814da2f0/8.8.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "validate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "validate_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "validate_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "validate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "validate_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "validate_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
