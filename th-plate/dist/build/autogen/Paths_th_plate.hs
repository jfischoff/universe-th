module Paths_th_plate (
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

bindir     = "/Volumes/Booyah/projects/game-tools/src/swifit/cabal-dev//bin"
libdir     = "/Volumes/Booyah/projects/game-tools/src/swifit/cabal-dev//lib"
datadir    = "/Volumes/Booyah/projects/game-tools/src/swifit/cabal-dev//share"
libexecdir = "/Volumes/Booyah/projects/game-tools/src/swifit/cabal-dev//libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "th_plate_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "th_plate_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "th_plate_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "th_plate_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
