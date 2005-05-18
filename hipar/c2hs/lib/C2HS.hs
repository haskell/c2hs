--  C->Haskell Compiler: global interface of marshaling library
--
--  Author : Manuel M. T. Chakravarty
--  Created: 19 August 99
--
--  Version $Revision: 1.7 $ from $Date: 2000/08/04 13:16:15 $
--
--  Copyright (c) 1999 Manuel M. T. Chakravarty
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
--- TODO ----------------------------------------------------------------------
--
--  * Get the marshaling stuff from GTK+HS.
--

module C2HS (Addr, nullAddr, plusAddr,
	     module C2HSConfig,
	     module C2HSBase,
	     module C2HSMarsh)
where 

import Addr       (Addr, nullAddr)
import qualified
       Addr   (plusAddr)	-- !!!for GHC 4.08

import C2HSConfig hiding (sizeofFloat, sizeofDouble, sizeofAddr)
import C2HSBase
import C2HSMarsh

-- !!!bad hack for GHC 4.08
plusAddr :: Addr -> Int -> Addr
plusAddr a o = Addr.plusAddr a (fromInteger . toInteger $ o)

