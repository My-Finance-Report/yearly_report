{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_year_report_finances (
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
bindir     = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/bin"
libdir     = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/lib/aarch64-osx-ghc-9.8.2/year-report-finances-0.1.0.0-741lZObJNamKdiYKdvkyVC-year-report-finances"
dynlibdir  = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/lib/aarch64-osx-ghc-9.8.2"
datadir    = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/share/aarch64-osx-ghc-9.8.2/year-report-finances-0.1.0.0"
libexecdir = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/libexec/aarch64-osx-ghc-9.8.2/year-report-finances-0.1.0.0"
sysconfdir = "/Users/matt/Projects/year_report_finances/.stack-work/install/aarch64-osx/68e93eb60f5bcc2966ce0ffbfcad9f96a625a3c0f54be9120ab47093e9c12652/9.8.2/etc"

getBinDir     = catchIO (getEnv "year_report_finances_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "year_report_finances_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "year_report_finances_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "year_report_finances_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "year_report_finances_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "year_report_finances_sysconfdir") (\_ -> return sysconfdir)



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
