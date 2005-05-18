--  Compiler Toolkit: Sets derived from finite maps
--
--  Author : Manuel M T Chakravarty
--  Created: 2 February 99
--
--  Version $Revision: 1.6 $ from $Date: 2003/04/16 11:11:46 $
--
--  Copyright (c) [1999..2003] Manuel M T Chakravarty
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
--  This module provides sets as an abstract data type implemented on top of
--  finite maps. 
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module Sets (
  Set, zeroSet, unitSet, listToSet, joinSet, sizeSet, addToSet,
  delFromSet, diffSet, isSubSet, isSuperSet, intersectSet, mapSet,
  foldSet, filterSet, elemSet, toListSet, powerSet,

  -- operations related to the underlying finite maps
  --
  domSetFM
) where

import FiniteMaps (FiniteMap, zeroFM, unitFM, listToFM, joinFM, 
		   joinCombFM, sizeFM, addToFM, delFromFM, diffFM,
		   intersectFM, foldFM, filterFM, lookupFM, lookupDftFM,
		   mapFM, toListFM)

-- a set is a finite map with a trivial image (EXPORTED ABSTRACT)
--
newtype (Ord a) => 
	Set a = Set (FiniteMap a ())
		deriving (Eq, Ord)

-- ATTENION: the ordering is _not_ the subset relation
--
instance (Show a, Ord a) => Show (Set a) where
  showsPrec = toShowS		-- defined below


zeroSet :: Ord a => Set a
zeroSet  = Set zeroFM

unitSet   :: Ord a => a -> Set a
unitSet x  = Set $ unitFM x ()

listToSet :: Ord a => [a] -> Set a
listToSet  = Set . listToFM . (map (\x -> (x, ())))

sizeSet         :: Ord a => Set a -> Int
sizeSet (Set s)  = sizeFM s

addToSet           :: Ord a => a -> Set a -> Set a
addToSet x (Set s)  = Set $ addToFM x () s

delFromSet           :: Ord a => a -> Set a -> Set a
delFromSet x (Set s)  = Set $ delFromFM x s

joinSet                 :: Ord a => Set a -> Set a -> Set a
joinSet (Set s) (Set t)  = Set $ joinFM s t

diffSet		        :: Ord a => Set a -> Set a -> Set a
diffSet (Set s) (Set t)  = Set $ diffFM s t

isSubSet       :: Ord a => Set a -> Set a -> Bool
isSubSet s1 s2  = s1 `diffSet` s2 == zeroSet

isSuperSet       :: Ord a => Set a -> Set a -> Bool
isSuperSet s1 s2  = s2 `isSubSet` s1

intersectSet                 :: Ord a => Set a -> Set a -> Set a
intersectSet (Set s) (Set t)  = Set $ intersectFM s t

mapSet           :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
mapSet f (Set s)  = Set $ (listToFM . map (\(x, _) -> (f x, ())) . toListFM) s

foldSet             :: Ord a => (a -> b -> b) -> b -> Set a -> b
foldSet f z (Set s)  = foldFM (\x _ y -> f x y) z s

filterSet           :: Ord a => (a -> Bool) -> Set a -> Set a
filterSet p (Set s)  = Set $ filterFM (\x _ -> p x) s

elemSet           :: Ord a => a -> Set a -> Bool
elemSet x (Set s)  = case lookupFM s x of
		       Nothing -> False
		       Just _  -> True

toListSet         :: Ord a => Set a -> [a]
toListSet (Set s)  = (map fst . toListFM) s

-- compute the power set of the given set (EXPORTED)
--
powerSet :: Ord a => Set a -> Set (Set a)
powerSet  = foldSet addOne (unitSet zeroSet)
	    where
	      addOne e s = mapSet (addToSet e) s `joinSet` s

-- pretty print routine (used as a method in the `Set' instance of `Show')
--
toShowS           :: (Show a, Ord a) => Int -> Set a -> ShowS
toShowS _ (Set s)  =   showString "{" 
		     . (format . map fst . toListFM $ s) 
		     . showString "}"
		     where
		       format []     = showString ""
		       format [x]    = shows x
		       format (x:xs) = shows x . showString ", " . format xs


-- Operations relating to the underlying finite maps
-- -------------------------------------------------

-- |Yield the domain of a finite map as a set
--
domSetFM :: Ord k => FiniteMap k e -> Set k
domSetFM = Set . mapFM (\_ _ -> ())
