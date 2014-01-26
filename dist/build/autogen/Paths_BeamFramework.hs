module Paths_BeamFramework (
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
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/kyle/Projects/BeamFramework/.hsenv/cabal/bin"
libdir     = "/Users/kyle/Projects/BeamFramework/.hsenv/cabal/lib/x86_64-osx-ghc-7.6.3/BeamFramework-0.1.0.0"
datadir    = "/Users/kyle/Projects/BeamFramework/.hsenv/cabal/share/x86_64-osx-ghc-7.6.3/BeamFramework-0.1.0.0"
libexecdir = "/Users/kyle/Projects/BeamFramework/.hsenv/cabal/libexec"
sysconfdir = "/Users/kyle/Projects/BeamFramework/.hsenv/cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "BeamFramework_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "BeamFramework_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "BeamFramework_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "BeamFramework_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "BeamFramework_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
