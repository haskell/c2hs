--  C->Haskell Compiler: information about the C implementation
--
--  Author : Manuel M T Chakravarty
--  Created: 5 February 01
--
--  Version $Revision: 1.6 $ from $Date: 2002/09/06 06:34:13 $
--
--  Copyright (c) 2001 Manuel M T Chakravarty
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
--  Bit fields
--  ~~~~~~~~~~
--  Bit fields in C can be signed and unsigned.  According to K&R A8.3, they
--  can only be formed from `int', `signed int', and `unsigned int', where for
--  `int' it is implementation dependent whether the field is signed or
--  unsigned.  Moreover, the following parameters are implementation
--  dependent:
--
--  * the direction of packing bits into storage units,
--  * the size of storage units, and
--  * whether when a field that doesn't fit a partially filled storage unit
--    is split across units or the partially filled unit is padded.
--
--  Generally, unnamed fields (those without an identifier) with a width of 0
--  are guaranteed to forces the above padding.  Note that in `CPrimType' we
--  only represent 0 width fields *if* they imply padding.  In other words,
--  whenever they are unnamed, they are represented by a `CPrimType', and if
--  they are named, they are represented by a `CPrimType' only if that
--  targeted C compiler chooses to let them introduce padding.  If a field
--  does not have any effect, it is dropped during the conversion of a C type
--  into a `CPrimType'-based representation.
--
--  In the code, we assume that the alignment of a bitfield (as determined by
--  `bitfieldAlignment') is independent of the size of the bitfield.
--
--- TODO ----------------------------------------------------------------------
--

module CInfo (
  CPrimType(..), size, alignment, 
  bitfieldDirection, bitfieldPadding, bitfieldIntSigned, bitfieldAlignment
) where 

import CForeign

-- we can't rely on the compiler used to compile c2hs already having the new
-- FFI, so this is system dependent
--
import C2HSConfig (Ptr, FunPtr, 
		   bitfieldDirection, bitfieldPadding, bitfieldIntSigned,
		   bitfieldAlignment)
import qualified  
       C2HSConfig as Storable
		  (Storable(sizeOf, alignment))


-- calibration of C's primitive types
-- ----------------------------------

-- C's primitive types (EXPORTED)
--
-- * `CFunPtrPT' doesn't occur in Haskell representations of C types, but we
--   need to know their size, which may be different from `CPtrPT'
--
data CPrimType = CPtrPT		-- void *
	       | CFunPtrPT	-- void *()
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
	       | CSFieldPT  Int -- signed bit field
	       | CUFieldPT  Int -- unsigned bit field
	       deriving (Eq)

-- size of primitive type of C (EXPORTED)
--
-- * negative size implies that it is a bit, not an octet size
--
size                :: CPrimType -> Int
size CPtrPT          = Storable.sizeOf (undefined :: Ptr ())
size CFunPtrPT       = Storable.sizeOf (undefined :: FunPtr ())
size CCharPT         = 1
size CUCharPT        = 1
size CSCharPT        = 1
size CIntPT          = Storable.sizeOf (undefined :: CInt)
size CShortPT        = Storable.sizeOf (undefined :: CShort)
size CLongPT         = Storable.sizeOf (undefined :: CLong)
size CLLongPT        = Storable.sizeOf (undefined :: CLLong)
size CUIntPT         = Storable.sizeOf (undefined :: CUInt)
size CUShortPT       = Storable.sizeOf (undefined :: CUShort)
size CULongPT        = Storable.sizeOf (undefined :: CULong)
size CULLongPT       = Storable.sizeOf (undefined :: CLLong)
size CFloatPT        = Storable.sizeOf (undefined :: CFloat)
size CDoublePT       = Storable.sizeOf (undefined :: CDouble)
size CLDoublePT      = Storable.sizeOf (undefined :: CLDouble)
size (CSFieldPT bs)  = -bs
size (CUFieldPT bs)  = -bs

-- alignment of C's primitive types (EXPORTED)
--
-- * more precisely, the padding put before the type's member starts when the
--   preceding component is a char
--
alignment                :: CPrimType -> Int
alignment CPtrPT          = Storable.alignment (undefined :: Ptr ())
alignment CFunPtrPT       = Storable.alignment (undefined :: FunPtr ())
alignment CCharPT         = 1
alignment CUCharPT        = 1
alignment CSCharPT        = 1
alignment CIntPT          = Storable.alignment (undefined :: CInt)
alignment CShortPT        = Storable.alignment (undefined :: CShort)
alignment CLongPT         = Storable.alignment (undefined :: CLong)
alignment CLLongPT        = Storable.alignment (undefined :: CLLong)
alignment CUIntPT         = Storable.alignment (undefined :: CUInt)
alignment CUShortPT       = Storable.alignment (undefined :: CUShort)
alignment CULongPT        = Storable.alignment (undefined :: CULong)
alignment CULLongPT       = Storable.alignment (undefined :: CULLong)
alignment CFloatPT        = Storable.alignment (undefined :: CFloat)
alignment CDoublePT       = Storable.alignment (undefined :: CDouble)
alignment CLDoublePT      = Storable.alignment (undefined :: CLDouble)
alignment (CSFieldPT bs)  = fieldAlignment bs
alignment (CUFieldPT bs)  = fieldAlignment bs

-- alignment constraint for a C bitfield
--
-- * gets the bitfield size (in bits) as an argument
--
-- * alignments constraints smaller or equal to zero are reserved for bitfield
--   alignments
--
-- * bitfields of size 0 always trigger padding; thus, they get the maximal
--   size
--
-- * if bitfields whose size exceeds the space that is still available in a
--   partially filled storage unit trigger padding, the size of a storage unit
--   is provided as the alignment constraint; otherwise, it is 0 (meaning it
--   definitely starts at the current position)
--
-- * here, alignment constraint /= 0 are somewhat subtle; they mean that is
--   the given number of bits doesn't fit in what's left in the current
--   storage unit, alignment to the start of the next storage unit has to be
--   triggered
--
fieldAlignment                      :: Int -> Int
fieldAlignment 0                     = - (size CIntPT - 1)
fieldAlignment bs | bitfieldPadding  = - bs
		  | otherwise        = 0
