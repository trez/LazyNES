module Paths_LazyNES (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/j/.cabal/bin"
libdir     = "/home/j/.cabal/lib/x86_64-linux-ghc-7.6.3/LazyNES-0.1"
datadir    = "/home/j/.cabal/share/x86_64-linux-ghc-7.6.3/LazyNES-0.1"
libexecdir = "/home/j/.cabal/libexec"
sysconfdir = "/home/j/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "LazyNES_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "LazyNES_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "LazyNES_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "LazyNES_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "LazyNES_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
