module Paths_prsr (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/jberges/git/prsr/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/bin"
libdir     = "/home/jberges/git/prsr/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/lib/x86_64-linux-ghc-7.10.3/prsr-0.1.0.0-KeI5IJaG2ym2Eek28fTvTe"
datadir    = "/home/jberges/git/prsr/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/share/x86_64-linux-ghc-7.10.3/prsr-0.1.0.0"
libexecdir = "/home/jberges/git/prsr/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/libexec"
sysconfdir = "/home/jberges/git/prsr/.stack-work/install/x86_64-linux/lts-5.16/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "prsr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "prsr_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "prsr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "prsr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "prsr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
