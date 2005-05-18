--  Compiler Toolkit: miscellaneous utility routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 8 February 95
--
--  Version $Revision: 1.12 $ from $Date: 2000/02/28 06:28:59 $
--
--  Copyright (c) [1995..2000], Manuel M. T. Chakravarty
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
--  This module provides miscellaneous utility routines used in different parts
--  of the Compiler Tookit.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module Utils (sort, sortBy, lookupBy, indentMultilineString, quantifySubject,
	      ordinal, Tag(..), mapMaybeM, mapMaybeM_, mapEitherM, mapEitherM_)
where

import List (find)


-- list operations
-- ---------------

-- naive sort for a list of which the elements are in the Ord class (EXPORTED)
--
sort      :: Ord a => [a] -> [a]
sort []    = []
sort (m:l) = (sort . filter (< m)) l ++ [m] ++ (sort . filter (>= m)) l 


-- naive sort for a list with explicit ordering relation (smaller than) 
-- (EXPORTED)
--
sortBy               :: (a -> a -> Bool) -> [a] -> [a]
sortBy _       []     = []
sortBy smaller (m:l)  =    (sortBy smaller . filter (`smaller` m)) l 
			++ [m] 
			++ (sortBy smaller . filter (not . (`smaller` m))) l 

-- generic lookup
--
lookupBy      :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x  = fmap snd . find (eq x . fst)


-- string operations
-- -----------------

-- string manipulation
--

-- indent the given multiline text by the given number of spaces
-- 
indentMultilineString   :: Int -> String -> String
indentMultilineString n  = unlines . (map (spaces++)) . lines
			   where
			     spaces = take n (repeat ' ')

-- aux. routines for output
--

-- given a number and a string containing the quantified subject, yields two
-- strings; one contains the quantified subject and the other contains ``is''
-- or ``are'' depending on the quantification (EXPORTED)
--
quantifySubject         :: Int -> String -> (String, String)
quantifySubject no subj  = (noToStr no ++ " " ++ subj 
			    ++ (if plural then "s" else ""),
			    if plural then "are" else "is")
			   where
			     plural = (no /= 1)

			     noToStr  0 = "no"
			     noToStr  1 = "one"
			     noToStr  2 = "two"
			     noToStr  3 = "three"
			     noToStr  4 = "four"
			     noToStr  5 = "five"
			     noToStr  6 = "six"
			     noToStr  7 = "seven"
			     noToStr  8 = "eight"
			     noToStr  9 = "nine"
			     noToStr 10 = "ten"
			     noToStr 11 = "eleven"
			     noToStr 12 = "twelve"
			     noToStr no = show no

-- stringfy a ordinal number (must be positive) (EXPORTED)
--
ordinal   :: Int -> String
ordinal n  = if n < 0 
	     then
	       error "FATAL ERROR: Utilis: ordinal: Negative number!"
	     else 
	       case (n `mod` 10) of
	         1 | n /= 11 -> show n ++ "st"
	         2 | n /= 12 -> show n ++ "nd"
	         3 | n /= 13 -> show n ++ "rd"
	         _           -> show n ++ "th"


-- tags
-- ----

-- tag values of a type are meant to define a mapping that collapses values
-- onto a single integer that are meant to be identified in comparisons etc
--
class Tag a where
  tag :: a -> Int


-- monad operations
-- ----------------

-- maps some monad operation into a `Maybe', yielding a monad
-- providing the mapped `Maybe' as its result (EXPORTED)
--
mapMaybeM            :: Monad m 
		     => (a -> m b) -> Maybe a -> m (Maybe b)
mapMaybeM m Nothing   = return Nothing
mapMaybeM m (Just a)  = m a >>= \r -> return (Just r)

-- like above, but ignoring the result (EXPORTED)
--
mapMaybeM_     :: Monad m => (a -> m b) -> Maybe a -> m ()
mapMaybeM_ m x  = mapMaybeM m x >> return ()

-- maps monad operations into a `Either', yielding a monad
-- providing the mapped `Either' as its result (EXPORTED)
--
mapEitherM               :: Monad m 
		         => (a -> m c) 
			 -> (b -> m d) 
			 -> Either a b 
			 -> m (Either c d)
mapEitherM m n (Left x)   = m x >>= \r -> return (Left r)
mapEitherM m n (Right y)  = n y >>= \r -> return (Right r)

-- like above, but ignoring the result (EXPORTED)
--
mapEitherM_ :: Monad m => (a -> m c) -> (b -> m d) -> Either a b -> m ()
mapEitherM_ m n x = mapEitherM m n x >> return ()
