module Paths_universe_th (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0,6], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/Users/hi5networks/.cabal/bin"
libdir     = "/Users/hi5networks/.cabal/lib/universe-th-0.0.0.6/ghc-7.2.2"
datadir    = "/Users/hi5networks/.cabal/share/universe-th-0.0.0.6"
libexecdir = "/Users/hi5networks/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "universe_th_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "universe_th_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "universe_th_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "universe_th_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
