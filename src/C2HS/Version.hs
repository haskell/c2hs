module C2HS.Version (versnum, version, copyright, disclaimer)          -- -*-haskell-*-
where

import qualified Paths_c2hs (version)
import Data.Version (Version, showVersion)

name, versnick, date, version, copyright, disclaimer :: String
versnum :: Version

name       = "C->Haskell Compiler"
versnum    = Paths_c2hs.version
versnick   = "Crystal Seed"
date       = "24 Jan 2009"
version    = name ++ ", version " ++ showVersion versnum ++ " " ++ versnick ++ ", " ++ date
copyright  = "Copyright (c) 1999-2007 Manuel M T Chakravarty\n"
          ++ "              2005-2008 Duncan Coutts\n"
          ++ "              2008      Benedikt Huber"
disclaimer = "This software is distributed under the \
             \terms of the GNU Public Licence.\n\
             \NO WARRANTY WHATSOEVER IS PROVIDED. \
             \See the details in the documentation."
