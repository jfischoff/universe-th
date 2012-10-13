module Paths_universe_th (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/jfischoff/Library/Haskell/lib//bin"
libdir     = "/Users/jfischoff/Library/Haskell/lib//lib/universe-th-0.1.0.0/x86_64/ghc-7.6.1"
datadir    = "/Users/jfischoff/Library/Haskell/lib//share/universe-th-0.1.0.0"
libexecdir = "/Users/jfischoff/Library/Haskell/lib//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "universe_th_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "universe_th_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "universe_th_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "universe_th_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
