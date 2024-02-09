{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_xo (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\bin"
libdir     = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\lib\\x86_64-windows-ghc-9.6.4\\xo-0.1.0.0-BhPfsr2V5MIDApYuCWxTTi-xo-exe"
dynlibdir  = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\lib\\x86_64-windows-ghc-9.6.4"
datadir    = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\share\\x86_64-windows-ghc-9.6.4\\xo-0.1.0.0"
libexecdir = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\libexec\\x86_64-windows-ghc-9.6.4\\xo-0.1.0.0"
sysconfdir = "C:\\Users\\user\\xo\\.stack-work\\install\\dcf43b5f\\etc"

getBinDir     = catchIO (getEnv "xo_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "xo_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "xo_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "xo_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "xo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "xo_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
