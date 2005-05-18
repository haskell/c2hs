--  C->Haskell Compiler: global interface of the marshalling library
--
--  Author : Manuel M T Chakravarty
--  Created: 19 August 99
--
--  Version $Revision: 1.11 $ from $Date: 2003/10/19 10:46:10 $
--
--  Copyright (c) [1999...2003] Manuel M T Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides the marshaling routines for Haskell files produced by 
--  C->Haskell for binding to C library interfaces.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The module exports all of the low-level FFI (language-independent plus the
--  C-specific parts) together with the C->HS-specific higher-level
--  marshalling routines.
--
--- TODO ----------------------------------------------------------------------
--

module C2HS (
  --
  -- re-export the language-independent component of the FFI 
  --
  module Foreign,
  --
  -- re-export the C language component of the FFI
  --
  module CForeign,
  --
  -- C->HS specific marshalling functionality
  --
  module C2HSBase,
  module C2HSMarsh
) where 


import Foreign
       hiding       (Word)
		    -- Should also hide the Foreign.Marshal.Pool exports in
		    -- compilers that export them
import CForeign

import C2HSBase
import C2HSMarsh
