#!/usr/bin/env runhaskell

import System (system, ExitCode)

import Distribution.Simple

-- for the argument types of the `postInst' hook
import Distribution.Setup
import Distribution.Simple.LocalBuildInfo

main = defaultMainWithHooks defaultUserHooks {postInst = addWrapperAndLib}

-- Install the c2hs shell script wrapper that passes the --data options as
-- well as the `C2HS' library module.
--
-- * We need to do this via a shell script that has been munged by ./configure,
--   as Cabal doesn't give us enough info (eg, no package name and version) to
--   do it all in Haskell.
--
addWrapperAndLib :: Args -> InstallFlags -> LocalBuildInfo -> IO ExitCode
addWrapperAndLib _ _ _ =
  system "./postInst.sh"
