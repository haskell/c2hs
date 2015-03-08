--
-- This Cabal setup script should ONLY be used in cases WHERE:
--
--   the bare structure wrapper capability of C2HS is used (i.e. '%'
--   characters on pointer type arguments in "fun" hooks);
--
-- AND
--
--   a version of Cabal is being used that does not allow
--   preprocessors to specify extra C sources (versions of
--   cabal-install <= 1.22.0.0).
--
-- Otherwise Cabal should be able to deal with the extra C sources
-- itself.
--
-- If in doubt, ask...
--

module Main (main) where

import Control.Exception (catch)
import Control.Monad (forM)

import Distribution.PackageDescription
    (BuildInfo(..), Executable(..), Library(..), PackageDescription(..))
import Distribution.Simple (UserHooks(..),
                            defaultMainWithHooks, simpleUserHooks)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Setup (BuildFlags)
-- Test-suites require Cabal-1.10 or greater
import Distribution.PackageDescription (TestSuite(..))
-- Benchmarks require Cabal-1.14 or greater
import Distribution.PackageDescription (Benchmark(..))

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.Exit (ExitCode)
import System.FilePath ((</>), takeExtensions)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { buildHook = chsBuildHook }

addCSources :: [FilePath] -> BuildInfo -> BuildInfo
addCSources newSrcs bi@(BuildInfo { cSources = oldSrcs }) =
  bi { cSources = newSrcs ++ oldSrcs }

hasChsCExtension :: FilePath -> Bool
hasChsCExtension file = takeExtensions file == ".chs.c"

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    topdirExists <- doesDirectoryExist topdir
    if (not topdirExists)
       then return []
       else do
           names <- getDirectoryContents topdir
           let properNames = filter (`notElem` [".", ".."]) names
           paths <- forM properNames $ \name -> do
               let path = topdir </> name
               isDirectory <- doesDirectoryExist path
               if isDirectory
                  then getRecursiveContents path
                  else return [path]
           return (concat paths)

chsBuildHook :: PackageDescription -> LocalBuildInfo -> UserHooks ->
                BuildFlags -> IO ()
chsBuildHook pd lbi uh bf = hook `catchExitCode` \_ -> hook
  where
    hook :: IO ()
    hook = chsBuildHook' pd lbi uh bf

    catchExitCode :: IO a -> (ExitCode -> IO a) -> IO a
    catchExitCode = catch

chsBuildHook' :: PackageDescription -> LocalBuildInfo -> UserHooks ->
                 BuildFlags -> IO ()
chsBuildHook' pd@(PackageDescription
                   { library     = mbLib
                   , executables = exes
                   , testSuites  = tss
                   , benchmarks  = bms
                   })
             lbi uh bf = do
    let distBuildDir = buildDir lbi
    chsCFiles <- fmap (filter hasChsCExtension) $
                 getRecursiveContents distBuildDir
    let pd' = pd { library     = fmap (\lib -> lib { libBuildInfo       = addCSources chsCFiles (libBuildInfo lib)      }) mbLib
                 , executables = fmap (\exe -> exe { buildInfo          = addCSources chsCFiles (buildInfo exe)         }) exes
                 , testSuites  = fmap (\ts  -> ts  { testBuildInfo      = addCSources chsCFiles (testBuildInfo ts)      }) tss
                 , benchmarks  = fmap (\bm  -> bm  { benchmarkBuildInfo = addCSources chsCFiles (benchmarkBuildInfo bm) }) bms
                 }
    buildHook simpleUserHooks pd' lbi uh bf
