--  C->Haskell Compiler: extra marshaling routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 October 99
--
--  Version $Revision: 1.18 $ from $Date: 2001/02/04 14:59:02 $
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
--  This module provides som extra marshalling routines that are used by C
--  interfaces produced with `c2hs'.  Not much left in here with the new FFI.
--  Partially, because the interface of `CString' (which in turn is determined
--  by the requirements of Unicode) doesn't leave any sensible uniform
--  treatment of lists without resorting to multiple parameter type classes.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module C2HSMarsh (
  -- conditional results using `Maybe'
  --
  nothingIf, nothingIfNull
) where

import Monad	    (liftM)
import Maybe        (isNothing)
import Ptr          (Ptr, nullPtr, castPtr)
import NewStorable  (Storable(..))
import MarshalUtils (new)


{-
-- Is the following useful at all?
type FromHaskell hsType cType = hsType -> IO (Ptr cType)
type ToHaskell   hsType cType = Ptr cTyp  -> IO hsType

fromInt :: Integral i => FromHaskell Int i
fromInt  = new . cIntConv

toInt   :: Integral i => ToHaskell Int i
toInt p  = do {r <- peek p; free p; return $ cIntConv}

fromFloat :: RealFloat i => FromHaskell Float i
fromFloat  = new . cFloatConv

toFloat   :: RealFloat i => ToHaskell Float i
toFloat p  = do {r <- peek p; free p; return r$ cFloatConv}

fromString :: FromHaskell String CChar
fromString  = newCString

toString   :: ToHaskell String CChar
toString p  = do {res <- peekCString p; free p; return res}
-}


-- storing of `Maybe' values
-- -------------------------

instance Storable a => Storable (Maybe a) where
  sizeOf    _ = sizeOf    (undefined :: Ptr ())
  alignment _ = alignment (undefined :: Ptr ())

  peek p = do
	     ptr <- peek (castPtr p)
	     if ptr == nullPtr
	       then return Nothing
	       else liftM Just $ peek ptr

  poke p v = do
	       ptr <- case v of
		        Nothing -> return nullPtr
			Just v' -> new v'
               poke (castPtr p) ptr


-- conditional results using `Maybe'
-- ---------------------------------

-- wrap the result into a `Maybe' type (EXPORTED)
--
-- * the predicate determines when the result is considered to be non-existing,
--   ie, it is represented by `Nothing'
--
-- * the second argument allows to map a result wrapped into `Just' to some
--   other domain
--
nothingIf       :: (a -> Bool) -> (a -> b) -> a -> Maybe b
nothingIf p f x  = if p x then Nothing else Just $ f x

-- instance for special casing null pointers (EXPORTED)
--
nothingIfNull :: (Ptr a -> b) -> Ptr a -> Maybe b
nothingIfNull  = nothingIf (== nullPtr)
