--  C -> Haskell Compiler: management of switches
--
--  Author : Manuel M. T. Chakravarty
--  Created: 6 March 99
--
--  Version $Revision: 1.9 $ from $Date: 2001/05/05 08:48:43 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
--  * The `hpaths' switch lists all directories that should be considered when
--    searching for a header file.
--
--  * The `keep' flag says whether the intermediate file produced by the C
--    pre-processor should be retained or not.
--
--  * Traces specify which trace information should be output by the compiler.
--    Currently the following trace information is supported:
--
--    - information about phase activation and phase completion
--
--  * The output path determines the file where the result of the compilation
--    is stored (it is supposed to include a suffix).  If it is empty, the
--    name of the Haskell binding file is used to determine the file name.
--
--  * With the advent of the new FFI in GHC 5.00 we adopt c2hs to fully support
--    these interfaces. To compile .chs files for earlier versions of GHC use
--    --new-ffi=no .
--
--- TODO ----------------------------------------------------------------------
--

module Switches (SwitchBoard(..), Traces(..), 
		 initialSwitchBoard)  
where


-- the switch board contains all toolkit switches
-- ----------------------------------------------

-- all switches of the toolkit (EXPORTED)
--
data SwitchBoard = SwitchBoard {
		     cppOptsSB :: String,	-- cpp options
		     cppSB     :: FilePath,	-- cpp executable
		     hpathsSB  :: [FilePath],	-- header file directories
		     keepSB    :: Bool,		-- keep intermediate file
		     tracesSB  :: Traces,	-- trace flags
		     outputSB  :: FilePath,	-- output file
		     oldFFI    :: Bool		-- GHC 4.XX compatible code
		   }

-- switch states on startup (EXPORTED)
--
initialSwitchBoard :: SwitchBoard
initialSwitchBoard  = SwitchBoard {
			cppOptsSB = "",
			cppSB     = "cpp",
			hpathsSB  = [],
			keepSB	  = False,
		        tracesSB  = initialTraces,
			outputSB  = "",
			oldFFI	  = False
		      }


-- traces
-- ------

-- different kinds of traces possible (EXPORTED)
--
data Traces = Traces {
	        tracePhasesSW  :: Bool,
	        traceGenBindSW :: Bool,
	        traceCTravSW   :: Bool,
		dumpCHSSW      :: Bool
	      }

-- trace setting on startup
--
-- * all traces are initially off
--
initialTraces :: Traces
initialTraces  = Traces {
		   tracePhasesSW  = False,
		   traceGenBindSW = False,
		   traceCTravSW   = False,
		   dumpCHSSW	  = False
		 }
