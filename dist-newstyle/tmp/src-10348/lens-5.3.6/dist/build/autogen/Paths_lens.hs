{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lens (
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
version = Version [5,3,6] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\bin"
libdir     = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\lib"
dynlibdir  = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\lib"
datadir    = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\share"
libexecdir = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\libexec"
sysconfdir = "C:\\cabal\\store\\ghc-9.12.2-4764\\lens-5.3.6-f0260a2fbabb4da506ec2aae7887f7d3eb0a564d\\etc"

getBinDir     = catchIO (getEnv "lens_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lens_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lens_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lens_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lens_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lens_sysconfdir") (\_ -> return sysconfdir)



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
