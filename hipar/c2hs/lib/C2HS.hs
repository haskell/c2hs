--  C->Haskell Compiler: global interface of the marshalling library
--
--  Author : Manuel M. T. Chakravarty
--  Created: 19 August 99
--
--  Version $Revision: 1.10 $ from $Date: 2001/11/14 09:08:12 $
--
--  Copyright (c) [1999...2000] Manuel M. T. Chakravarty
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
--  * Get the marshaling stuff from GTK+HS.
--

module C2HS (
  --
  -- re-export the language-independent component of the FFI 
  --
--  module Foreign, -- have to be more precise while supporting old systems
  module Bits,
  module Int,
  module Word,
  module Ptr,
  module ForeignPtr,
  module NewStablePtr, -- compensates old types in old versions of `StablePtr'
  module NewStorable,  -- compensates old types in old versions of `Storable'
  module MarshalAlloc,
  module MarshalArray,
  module MarshalError,
  module MarshalUtils,
  --
  -- re-export the C language component of the FFI
  --
  module CForeign,
  --
  -- re-export from IOExts
  --
  unsafePerformIO,
  --
  -- C->HS specific marshalling functionality
  --
  module C2HSBase,
  module C2HSMarsh
) where 


import Bits	    (Bits(..), shiftL, shiftR, rotateL, rotateR)
import Int          (Int8, Int16, Int32, Int64)
import Word	    (Word8, Word16, Word32, Word64)
import Ptr	    (Ptr, nullPtr, castPtr, plusPtr, alignPtr, minusPtr,
		     FunPtr, nullFunPtr, castFunPtr, castFunPtrToPtr,
		     castPtrToFunPtr, freeHaskellFunPtr)
import ForeignPtr   (ForeignPtr, newForeignPtr, addForeignPtrFinalizer,
		     withForeignPtr, foreignPtrToPtr, touchForeignPtr,
		     castForeignPtr)
import NewStablePtr (StablePtr, newStablePtr, deRefStablePtr, freeStablePtr, 
		     castStablePtrToPtr, castPtrToStablePtr)
import NewStorable  (Storable(..))
import MarshalAlloc (malloc, mallocBytes, alloca, allocaBytes, reallocBytes,
		     free)
import MarshalArray (mallocArray, mallocArray0, allocaArray, allocaArray0, 
		     reallocArray, reallocArray0, peekArray, peekArray0,
		     pokeArray, pokeArray0, newArray, newArray0, withArray,
		     withArray0, copyArray, moveArray, advancePtr)
import MarshalError (throwIf, throwIf_, throwIfNeg, throwIfNeg_, throwIfNull,
		     void)
import MarshalUtils (withObject, new, fromBool, toBool,	maybeNew, maybeWith,
		     maybePeek, withMany, copyBytes, moveBytes)

import CForeign

import IOExts       (unsafePerformIO)

import C2HSBase
import C2HSMarsh
