{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_PortMidi (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/bin"
libdir     = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/lib/x86_64-osx-ghc-8.10.4/PortMidi-0.2.0.0-2WYagIx0IRd1VpSqRi4Dni"
dynlibdir  = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/lib/x86_64-osx-ghc-8.10.4"
datadir    = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/share/x86_64-osx-ghc-8.10.4/PortMidi-0.2.0.0"
libexecdir = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/libexec/x86_64-osx-ghc-8.10.4/PortMidi-0.2.0.0"
sysconfdir = "/Users/almehdikrisni/Desktop/UPMC/L3 INFO/Semestre 2/Prog_Comparee (LU3IN032)/MozartProjet_NV/MozartGame/.stack-work/install/x86_64-osx/38e4d642e27e5a7fba80ddf89bb78fa3eac00a03dcd4e744ff6ee5cccab93555/8.10.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "PortMidi_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "PortMidi_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "PortMidi_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "PortMidi_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "PortMidi_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "PortMidi_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
