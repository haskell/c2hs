--  The Compiler Toolkit: difference lists
--
--  Author : Manuel M. T. Chakravarty
--  Created: 24 February 95
--
--  Copyright (c) [1995..2000] Manuel M. T. Chakravarty
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
--  This module provides the functional equivalent of the difference lists
--  from logic programming.  They provide an O(1) append.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module Data.DLists (DList, open, zero, unit, snoc, join, close)
where

-- | a difference list is a function that given a list returns the original
-- contents of the difference list prepended at the given list
--
type DList a = [a] -> [a]

-- | open a list for use as a difference list
--
open :: [a] -> DList a
open  = (++)

-- | create a difference list containing no elements
--
zero :: DList a
zero  = id

-- | create difference list with given single element
--
unit :: a -> DList a
unit  = (:)

-- | append a single element at a difference list
--
snoc :: DList a -> a -> DList a
snoc dl x  = \l -> dl (x:l)

-- | appending difference lists
--
join :: DList a -> DList a -> DList a
join  = (.)

-- | closing a difference list into a normal list
--
close :: DList a -> [a]
close  = ($[])
