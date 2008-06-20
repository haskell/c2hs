module C2HS.Version (versnum, version, copyright, disclaimer)          -- -*-haskell-*-
where

import qualified Paths_c2hs (version)
import Data.Version (showVersion)

name       = "C->Haskell Compiler"
versnum    = Paths_c2hs.version
versnick   = "Rainy Days"
date       = "31 Aug 2007"
version    = name ++ ", version " ++ showVersion versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) [1999..2007] Manuel M T Chakravarty"
disclaimer = "This software is distributed under the \
             \terms of the GNU Public Licence.\n\
             \NO WARRANTY WHATSOEVER IS PROVIDED. \
             \See the details in the documentation."
