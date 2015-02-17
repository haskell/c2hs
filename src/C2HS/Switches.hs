--  C -> Haskell Compiler: management of switches
--
--  Author : Manuel M T Chakravarty
--  Created: 6 March 99
--
--  Copyright (c) [1999..2005] Manuel M T Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module manages C2HS's compiler switches. It exports the data types
--  used to store the switches and operations on them.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Overview over the switches:
--
--  * The cpp options specify the options passed to the C preprocessor.
--
--  * The cpp filename gives the name of the executable of the C preprocessor.
--
--  * The `keep' flag says whether the intermediate file produced by the C
--    pre-processor should be retained or not.
--
--  * `platformSB' specifies the implementation-dependent parameters of the
--    targeted C compiler (as far as they are relevant to c2hs); this includes
--    especially the conventions for the memory layout of bitfields
--
--  * Traces specify which trace information should be output by the compiler.
--    Currently the following trace information is supported:
--
--    - information about phase activation and phase completion
--
--  * After processing the compiler options, `outputSB' contains the base name
--    for the generated Haskell, C header, and .chi files.  However, during
--    processing compiler options, `outputSB' contains arguments to the
--    `--output' option and `outDirSB' contains arguments to the
--    `--output-dir' option.
--
--- TODO ----------------------------------------------------------------------
--

module C2HS.Switches (
  SwitchBoard(..), Traces(..), initialSwitchBoard
) where

import C2HS.Config (PlatformSpec, defaultPlatformSpec)


-- the switch board contains all toolkit switches
-- ----------------------------------------------

-- | all switches of the toolkit
--
data SwitchBoard = SwitchBoard {
  cppOptsSB :: [String],      -- cpp options
  cppSB     :: FilePath,      -- cpp executable
  noGnuSB    :: Bool,         -- suppress GNU preproc. symbols
  noBlocksSB :: Bool,         -- suppress MacOS __BLOCKS__ symbol
  keepSB    :: Bool,          -- keep intermediate file
  librarySB :: Bool,          -- copy library in
  tracesSB  :: Traces,        -- trace flags
  outputSB  :: FilePath,      -- basename of generated files
  outDirSB  :: FilePath,      -- dir where generated files go
  platformSB:: PlatformSpec,  -- target platform spec.
  headerSB  :: FilePath,      -- generated header file
  chiPathSB :: [FilePath]     -- .chi file directories
  }

-- | switch states on startup
--
initialSwitchBoard :: SwitchBoard
initialSwitchBoard  = SwitchBoard {
                        cppOptsSB  = [],
                        cppSB      = "cpp",
                        noGnuSB    = False,
                        noBlocksSB = False,
                        keepSB     = False,
                        librarySB  = False,
                        tracesSB   = initialTraces,
                        outputSB   = "",
                        outDirSB   = "",
                        platformSB = defaultPlatformSpec,
                        headerSB   = "",
                        chiPathSB  = ["."]
                      }


-- traces
-- ------

-- | different kinds of traces possible
--
data Traces = Traces {
                tracePhasesSW  :: Bool,
                traceGenBindSW :: Bool,
                traceCTravSW   :: Bool,
                dumpCHSSW      :: Bool
              }

-- | trace setting on startup
--
-- * all traces are initially off
--
initialTraces :: Traces
initialTraces  = Traces {
                   tracePhasesSW  = False,
                   traceGenBindSW = False,
                   traceCTravSW   = False,
                   dumpCHSSW      = False
                 }
