--  C->Haskell Compiler: information about the C implementation
--
--  Author : Manuel M. T. Chakravarty
--  Created: 5 February 01
--
--  Version $Revision: 1.3 $
--
--  Copyright (c) 2001 Manuel M. T. Chakravarty
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
--  This module provide some information about the specific implementation of
--  C that we are dealing with.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module CInfo (
  CPrimType(..), sizes, alignments
) where 

import Array (Array, Ix, array)

import CTypes

-- we can't rely on the compiler used to compile c2hs already having the new
-- FFI, so this is system dependent
--
import C2HSConfig (Ptr,
		   Storable(sizeOf, alignment))


-- calibration of C's primitive types
-- ----------------------------------

-- C's primitive types (EXPORTED)
--
data CPrimType = CAddrPT	-- void *
	       | CFunAddrPT	-- void *
	       | CCharPT	-- char
	       | CUCharPT	-- unsigned char
	       | CSCharPT	-- signed char
	       | CIntPT		-- int
	       | CShortPT	-- short int
	       | CLongPT	-- long int
	       | CLLongPT	-- long long int
	       | CUIntPT	-- unsigned int
	       | CUShortPT	-- unsigned short int
	       | CULongPT	-- unsigned long int
	       | CULLongPT	-- unsigned long long int
	       | CFloatPT	-- float
	       | CDoublePT	-- double
	       | CLDoublePT	-- long double
	       deriving (Bounded, Eq, Ord, Ix)

-- sizes of C's primitive types (EXPORTED)
--
sizes :: Array CPrimType Int
sizes  = array (minBound, maxBound) [
           (CAddrPT   , sizeOf (undefined :: Ptr ())),
-- FIXME: this should be FunPtr in compilers with the new FFI
	   (CFunAddrPT, sizeOf (undefined :: {-Fun-}Ptr ())),
	   (CCharPT   , 1),
	   (CUCharPT  , 1),
	   (CSCharPT  , 1),
	   (CIntPT    , sizeOf (undefined :: CInt)),
	   (CShortPT  , sizeOf (undefined :: CShort)),
	   (CLongPT   , sizeOf (undefined :: CLong)),
	   (CLLongPT  , sizeOf (undefined :: CLLong)),
	   (CUIntPT   , sizeOf (undefined :: CUInt)),
	   (CUShortPT , sizeOf (undefined :: CUShort)),
	   (CULongPT  , sizeOf (undefined :: CULong)),
	   (CULLongPT , sizeOf (undefined :: CLLong)),
	   (CFloatPT  , sizeOf (undefined :: CFloat)),
	   (CDoublePT , sizeOf (undefined :: CDouble)),
	   (CLDoublePT, sizeOf (undefined :: CLDouble))
	 ]

-- alignment of C's primitive types (EXPORTED)
--
-- * more precisely, the padding put before the type's member starts when the
--   preceding component is a char
--
alignments :: Array CPrimType Int
alignments  = array (minBound, maxBound) [
                (CAddrPT   , alignment (undefined :: Ptr ())),
-- FIXME: this should be FunPtr in compilers with the new FFI
	        (CFunAddrPT, alignment (undefined :: {-Fun-}Ptr ())),
		(CCharPT   , 1),
		(CUCharPT  , 1),
		(CSCharPT  , 1),
		(CIntPT    , alignment (undefined :: CInt)),
		(CShortPT  , alignment (undefined :: CShort)),
		(CLongPT   , alignment (undefined :: CLong)),
		(CLLongPT  , alignment (undefined :: CLLong)),
		(CUIntPT   , alignment (undefined :: CUInt)),
		(CUShortPT , alignment (undefined :: CUShort)),
		(CULongPT  , alignment (undefined :: CULong)),
		(CULLongPT , alignment (undefined :: CULLong)),
		(CFloatPT  , alignment (undefined :: CFloat)),
		(CDoublePT , alignment (undefined :: CDouble)),
		(CLDoublePT, alignment (undefined :: CLDouble))
	      ]
