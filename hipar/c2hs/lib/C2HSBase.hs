{-# OPTIONS -fglasgow-exts #-}

--  C->Haskell Compiler: basic marshaling routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 27 September 99
--
--  Version $Revision: 1.10 $ from $Date: 2004/05/15 08:34:50 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
--  This module provides the basic marshaling routines that are used by C
--  interfaces produced with `c2hs'.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--
--  * There should be two versions of this library, where one does bounds
--    checking on all lossy conversions.
--

module C2HSBase (
  --
  -- conversion between C and Haskell types
  --
  cIntConv, cFloatConv, cToBool, cFromBool, cToEnum, cFromEnum
) where

import Monad        (when, liftM)

import MarshalUtils (fromBool, toBool)


-- conversion routines
-- -------------------

-- integral conversion (EXPORTED)
--
cIntConv :: (Integral a, Integral b) => a -> b
cIntConv  = fromIntegral

-- floating conversion (EXPORTED)
--
cFloatConv :: (RealFloat a, RealFloat b) => a -> b
cFloatConv  = realToFrac
-- As this conversion by default goes via `Rational', it can be very slow...
{-# RULES 
  "cFloatConv/Float->Float"   forall (x::Float).  cFloatConv x = x;
  "cFloatConv/Double->Double" forall (x::Double). cFloatConv x = x
 #-}

-- obtain C value from Haskell `Bool' (EXPORTED)
--
cFromBool :: Num a => Bool -> a
cFromBool  = fromBool

-- obtain Haskell `Bool' from C value (EXPORTED)
--
cToBool :: Num a => a -> Bool
cToBool  = toBool

-- convert a C enumeration to Haskell (EXPORTED)
--
cToEnum :: (Integral i, Enum e) => i -> e
cToEnum  = toEnum . cIntConv

-- convert a Haskell enumeration to C (EXPORTED)
--
cFromEnum :: (Enum e, Integral i) => e -> i
cFromEnum  = cIntConv . fromEnum
