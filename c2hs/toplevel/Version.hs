module Version (version, copyright, disclaimer)		       -- -*-haskell-*-
where

import qualified Paths_c2hs (version)
import Data.Version (Version(versionBranch), showVersion)

name       = "C->Haskell Compiler"
versnum    = showVersion Paths_c2hs.version
versnick   = "Travelling Lightly"
date	   = "28 Apr 2006"
version    = name ++ ", version " ++ versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2006] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
	     \terms of the GNU Public Licence.\n\
	     \NO WARRANTY WHATSOEVER IS PROVIDED. \
	     \See the details in the documentation."
