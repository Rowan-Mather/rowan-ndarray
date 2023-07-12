{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_text (
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
version = Version [2,0,1] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/bin"
libdir     = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/lib"
dynlibdir  = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/lib"
datadir    = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/share"
libexecdir = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/libexec"
sysconfdir = "/home/rowan/.local/state/cabal/store/ghc-8.0.2/text-2.0.1-8ee3f50ff8174737a7d061d539e3e74efbf62e06dd5ed2e15eacab9e0e16a890/etc"

getBinDir     = catchIO (getEnv "text_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "text_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "text_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "text_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
