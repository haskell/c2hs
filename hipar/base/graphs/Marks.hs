--  Compiler Toolkit: efficient marking of attributed entities
--
--  Author : Manuel M. T. Chakravarty
--  Created: 6 December 1999
--
--  Version $Revision: 1.2 $ from $Date: 2001/02/07 09:24:46 $
--
--  Copyright (c) 1999 Manuel M. T. Chakravarty
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
--  This modules provides an abstract interface to a marking facility for
--  attributed entities.  Such a collection of marks can be regarded as a set, 
--  or as a marks as used when walking graphs.
--
--  This module is based on a suggestion from Roman Lechtchinsky.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * Collections of marks are parametrised to constrain one collection to
--    marks for only one type of entities.
--
--  * Currently, a very simple implementation based on sets is used.  The
--    interface can, however, be instantiated with a more efficient (eg,
--    hash-based) implementation if the need arises.
--
--- TODO ----------------------------------------------------------------------
--

module Marks (Marks, newMarks, mark, isMarked) 
where

import Sets	  (Set, zeroSet, addToSet, elemSet)
import Attributes (Attrs, Attributed(..))


-- representation of a collection of marks (EXPORTED ABSTRACTLY)
--
data Attributed a => Marks a = Marks (Set Attrs)
--newtype Attributed a => Marks a = Marks (Set Attrs)
-- should be newtype, but nhc98 chokes on it...

-- get a new collection of marks (EXPORTED)
--
newMarks :: Attributed a => Marks a
newMarks  = Marks zeroSet

-- mark an entity in a specific collection of marks (EXPORTED)
--
mark              :: Attributed a => Marks a -> a -> Marks a
mark (Marks ms) e  = Marks $ addToSet (attrsOf e) ms

-- test whether a given entity is marked in a given collection of marks
-- (EXPORTED) 
--
isMarked              :: Attributed a => Marks a -> a -> Bool
isMarked (Marks ms) e  = (attrsOf e) `elemSet` ms
