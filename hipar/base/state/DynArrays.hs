--  The HiPar Toolkit: dynamic arrays
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 October 96
--
--  Version $Revision: 1.11 $ from $Date: 1998/11/13 01:34:45 $
--
--  Copyright (C) [1996..1998] Manuel M. T. Chakravarty
--
--  THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU PUBLIC LICENCE
--  NO WARRANTY WHATSOEVER IS PROVIDED
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module realizes interger-indexed mutable arrays, which can grow
--  dynamically on top of the mutables provided by the `State' module.
--
--  Code using this module is bound to be programmed in a rather low-level
--  style by fiddling around with indicies to dynamically growing arrays --
--  which are basically a prettier name for raw memory. Such code is very
--  error-prone. So, use this module only if ABSOLUTELY necessary!
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 1.4
--
--  * Each array maintains a counter that allows a continuous allocation of new
--    slots in the array.
--
--  * When an array becomes full, a new array with twice the size is allocated
--    and the data copied into the new one. To be able to substitute the new
--    array easily for the old one, the dynamic array data structure stores the
--    reference to the mutable array within a mutable variable
--
--  * Both equality and an ordering is defined on slots (i.e., `DASlot' is an
--    instance of both `Eq' and `Ord').
--
--- TODO ----------------------------------------------------------------------
--

module DynArrays (DynArr, DASlot,
		  newDA, newDAS, readDA, writeDA, sizeDA, slotsDA)
where

import Common (assert)
import State  (PreCST,
	       yield, nop,
	       MArr, MVar,
	       newMV, newMA, readMV, assignMV, readMA, writeMA, boundsMA)


-- dynamic arrays (EXPORTED ABSTRACT)
--
data DynArr a = DynArr (MVar (Int,		-- next free slot
			      MArr Int a))	-- mutable data

-- reference to a slot of a dynamic array (EXPORTED ABSTRACT)
--
newtype DASlot = DASlot Int

instance Eq DASlot where
  (DASlot i) == (DASlot j) = i == j

instance Ord DASlot where
  (DASlot i) <= (DASlot j) = i <= j

instance Show DASlot where			-- for debugging
  showsPrec p (DASlot i) = showsPrec p i

-- number of slots allocated initially
--
initSize :: Int
initSize  = 32

-- generate a new dynamic array (it has no allocated slots in the beginning)
--
-- * accessing a not-yet-allocated slot raises an error
--
newDA :: PreCST e s (DynArr a)
newDA  = newMA (0, initSize - 1) err		>>= \ma ->
	 newMV (0, ma)				>>= \daRef ->
	 yield (DynArr daRef)
	 where
	   err = error "DynArrays: newDA: Accessed not-yet-allocated slot!"
	   

-- given a dynamic array, a new slot is generated and its index returned
--
newDAS                :: (DynArr a) -> PreCST e s DASlot
newDAS (DynArr daRef)  = 
  readMV daRef					>>= \(k, ma) ->
  let 
    (_, size) = boundsMA ma
  in
    if k < size
    then					-- still fits
      daRef `assignMV` (k + 1, ma)		>>
      writeMA ma k errUndef			>> 
      yield (DASlot k)
    else					-- alloc array twice the size
      newMA (0, 2 * size - 1) err		>>= \ma' ->
      copy ma ma' (k - 1)			>>
      daRef `assignMV` (k + 1, ma')		>>
      writeMA ma k errUndef			>> 
      yield (DASlot k)
   where
     -- copy :: (DynArr a) -> (DynArr a) -> Int -> PreCST e s ()
     copy ma ma' (-1) = nop
     copy ma ma' i    = readMA ma i		>>= \x ->
		        writeMA ma' i x		>>
		        copy ma ma' (i - 1)

     err      = error "DynArrays: newDAS: Accessed not-yet-allocated slot!"
     errUndef = error "DynArrays: newDAS: Accessed uninitialized slot!"

-- get the value from a slot of a dynamic array
--
readDA                  :: DynArr a -> DASlot -> PreCST e s a
readDA (DynArr daRef) s  =
  let
    DASlot s' = s
  in
  readMV daRef					>>= \(k, ma) ->
  assert (s' < k) "DynArrays: readDA: Out of bounds!"
  (
    readMA ma s'
  )

-- write a value into a slot of a dynamic array
--
writeDA                    :: DynArr a -> DASlot -> a -> PreCST e s ()
writeDA (DynArr daRef) s x  =
  let
    DASlot s' = s
  in
  readMV daRef					>>= \(k, ma) ->
  assert (s' < k)  "DynArrays: writeDA: Out of bounds!"
  (
    writeMA ma s' x
  )

-- yield the number of allocated slots of a dynamic array
--
sizeDA                :: DynArr a -> PreCST e s Int
sizeDA (DynArr daRef)  =
  readMV daRef					>>= \(k, _) ->
  yield k  

-- yield a list of all allocated slots of a dynamic array
--
slotsDA                :: DynArr a -> PreCST e s [DASlot]
slotsDA (DynArr daRef)  =
  readMV daRef					>>= \(k, _) ->
  yield (map DASlot [0..k - 1])

