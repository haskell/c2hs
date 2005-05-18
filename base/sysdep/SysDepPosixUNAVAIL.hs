--  Compiler Toolkit: posix dependent stuff - EMPTY STUB
--
--  Author : Manuel M. T. Chakravarty
--  Created: 18 August 2000
--
--  Version $Revision: 1.2 $ from $Date: 2001/02/07 09:24:48 $
--
--  Copyright (c) 2000 Manuel M. T. Chakravarty
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
--  This module provides stub routines for building on a system configuration
--  where the `posix' package from HSLibs is not available.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * may only import `Config'
--
--  Provides the same symbols as `SysDepPosixAVAIL', but without an
--  implementation.
--
--- TODO ----------------------------------------------------------------------
--

module SysDepPosix (
  ProcessID,		-- re-exported
  runPiped
) where

import IO (Handle)

-- definition doesn't matter as it isn't used anyway
--
type ProcessID = ()


-- Process management
-- ------------------

runPiped :: FilePath			    -- command
         -> [String]			    -- arguments
         -> Maybe [(String, String)]	    -- environment
         -> Maybe FilePath		    -- working directory    
         -> IO (ProcessID,Handle,Handle)    -- (child pid, fromChild, toChild)
runPiped _ _ _ _ =
  error "SysDepPosix.runPiped: not supported on this system"
