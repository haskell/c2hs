{-# LANGUAGE CPP #-}
--  C->Haskell Compiler: information about the C implementation
--
--  Author : Manuel M T Chakravarty
--  Created: 5 February 01
--
--  Copyright (c) [2001..2005] Manuel M T Chakravarty
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

module C2HS.C.Info (
  CPrimType(..)
) where


-- calibration of C's primitive types
-- ----------------------------------

-- | C's primitive types
--
-- * 'CFunPtrPT' doesn't occur in Haskell representations of C types, but we
--   need to know their size, which may be different from 'CPtrPT'
--
data CPrimType = CPtrPT         -- void *
               | CFunPtrPT      -- void *()
               | CCharPT        -- char
               | CUCharPT       -- unsigned char
               | CSCharPT       -- signed char
               | CIntPT         -- int
               | CShortPT       -- short int
               | CLongPT        -- long int
               | CLLongPT       -- long long int
               | CUIntPT        -- unsigned int
               | CUShortPT      -- unsigned short int
               | CULongPT       -- unsigned long int
               | CULLongPT      -- unsigned long long int
               | CFloatPT       -- float
               | CDoublePT      -- double
               | CLDoublePT     -- long double
               | CBoolPT        -- bool (C99 _Bool)
               | CSFieldPT  Int -- signed bit field
               | CUFieldPT  Int -- unsigned bit field
               | CAliasedPT String String CPrimType
               deriving (Eq, Show)
