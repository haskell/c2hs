--  Compiler Toolkit: finite maps
--
--  Author : Manuel M. T. Chakravarty
--  Created: 23 March 95
--
--  Version $Revision: 1.11 $ from $Date: 2000/04/15 13:33:00 $
--
--  Copyright (c) [1995..2000] Manuel M. T. Chakravarty
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
--  This module provides finite maps as an abstract data type. The idea is
--  taken from the GHC module `FiniteMap' and the implementation follows
--  closely the ideas found in ``Efficient sets---a balancing act'' from
--  Stephan Adams in ``Journal of Functional Programming'', 3(4), 1993,
--  drawing also from the longer exposition in ``Implementing Sets Efficiently
--  in a Functional Language'' also from Stephan Adams, CSTR 92-10 in Technical
--  Report Series, Unversity of Southampton, Department of Electronics and
--  Computer Science, U.K.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * This implementation is based in bounded balance binary trees. They 
--    achieve good balancing while being simpler to maintain than AVL trees.
--
--  * The implementation design is based on the idea of smart constructors,
--    i.e., constructors that guarantee the compliance of the result with some 
--    constraints applied to the construction of the data type.
--
--- TODO ----------------------------------------------------------------------
--
--  * `joinFM' would be a bit more efficient if the ``hedge union'' algorithm
--    of the above mentioned technical report would be implemented.
--

module FiniteMaps (FiniteMap, zeroFM, unitFM, listToFM, listToCombFM, joinFM, 
		   joinCombFM, sizeFM, addToFM, addToCombFM, delFromFM, diffFM,
		   intersectFM, intersectCombFM, mapFM, foldFM, filterFM, 
		   lookupFM, lookupDftFM, toListFM)
where

-- finite maps are represented as ordered binary trees; each node represents
-- a key-element pair in the map, its children contain pair with smaller and
-- greater keys respectively (this requires an ordering relation on the keys);
-- all keys in a tree are distinct
--
data (Ord key) =>
     FiniteMap key elem = Leaf
	                | Node key			-- this key
			       elem			-- assoc with key
			       Int			-- size >= 1
			       (FiniteMap key elem)	-- smaller keys
			       (FiniteMap key elem)	-- greater keys



-- we define two finite maps to be equal if they range over the same domain
--
--instance Ord k => Eq (FiniteMap k e) where
--  fm1 == fm2 = ((map fst . toListFM) $ fm1) == ((map fst . toListFM) $ fm2)
instance (Ord k, Eq e) => Eq (FiniteMap k e) where
  fm1 == fm2 = (toListFM fm1) == (toListFM fm2)

-- we define a total ordering on finite maps by lifting the lexicographical
-- ordering over their domains (which we assume to be sorted)
--
--instance Ord k => Ord (FiniteMap k e) where
--  fm1 <= fm2 = ((map fst . toListFM) $ fm1) <= ((map fst . toListFM) $ fm2)
instance (Ord k, Ord e) => Ord (FiniteMap k e) where
  fm1 <= fm2 = (toListFM fm1) <= (toListFM fm2)

instance (Show k, Show e, Ord k) => Show (FiniteMap k e) where
  showsPrec = toShowS		-- defined below


-- weight ratio is respected by the balanced tree, i.e., no subtree will ever
-- contain `ratio' times more elements than its sister
--
ratio :: Int
ratio  = 5

-- this gives us an empty map
--
zeroFM :: Ord k => FiniteMap k e
zeroFM  = Leaf

-- a map with a single element
--
unitFM     :: Ord k => k -> e -> FiniteMap k e
unitFM k e  = Node k e 1 Leaf Leaf

-- makes a list of key-element pairs into a finite map
--
-- in case of duplicates, the last is taken
-- 
listToFM :: Ord k => [(k, e)] -> FiniteMap k e
listToFM  = listToCombFM const

-- makes a list of key-element pairs into a finite map where collisions are 
-- resolved by an explicit combiner fun
--
-- the combiner expects the new element as its first argument
-- 
listToCombFM   :: Ord k => (e -> e -> e) -> [(k, e)] -> FiniteMap k e
listToCombFM c  = foldl addOnePair zeroFM
		  where
		    addOnePair m (k, e) = addToCombFM c k e m

-- the number of elements in the map
--
sizeFM                  :: Ord k => FiniteMap k e -> Int
sizeFM Leaf              = 0
sizeFM (Node _ _ s _ _)  = s

-- builds a node that automagically contains the right size 
--
smartNode :: Ord k
	  => k -> e -> (FiniteMap k e) -> (FiniteMap k e) -> (FiniteMap k e)
smartNode k e sm gr = Node k e (1 + sizeFM sm + sizeFM gr) sm gr

-- builds a node that automagically balances the tree if necessary and inserts
-- the right size; ONLY ONE of the subtrees is allowed to be off balance and
-- only by ONE element
--
smarterNode :: Ord k 
	    => k -> e -> (FiniteMap k e) -> (FiniteMap k e) -> (FiniteMap k e)
smarterNode k e sm gr =
  let
    sm_n = sizeFM sm
    gr_n = sizeFM gr
  in
    if (sm_n + gr_n) < 2	-- very small tree (one part is a leaf)
    then 
      smartNode k e sm gr	-- => construct directly
    else 
    if gr_n > (ratio * sm_n)    -- child with greater keys is too big
    then			-- => rotate left
      let
        Node _ _ _ gr_sm gr_gr = gr
	gr_sm_n		       = sizeFM gr_sm
	gr_gr_n		       = sizeFM gr_gr
      in
        if gr_sm_n < gr_gr_n then single_L k e sm gr else double_L k e sm gr
    else
    if sm_n > (ratio * gr_n)    -- child with smaller keys is too big
    then			-- => rotate right
      let
        Node _ _ _ sm_sm sm_gr = sm
	sm_sm_n		       = sizeFM sm_sm
	sm_gr_n		       = sizeFM sm_gr
      in
        if sm_gr_n < sm_sm_n then single_R k e sm gr else double_R k e sm gr
    else
      smartNode k e sm gr	-- else nearly balanced => construct directly
  where
    single_L ka ea x (Node kb eb _ y z) = smartNode kb eb 
						    (smartNode ka ea x y) 
						    z
    double_L ka ea x (Node kc ec _ (Node kb eb _ y1 y2) z) =
					  smartNode kb eb
						    (smartNode ka ea x  y1)
						    (smartNode kc ec y2 z)
    single_R kb eb (Node ka ea _ x y) z = smartNode ka ea
						    x
						    (smartNode kb eb y z) 
    double_R kc ec (Node ka ea _ x (Node kb eb _ y1 y2)) z =
					  smartNode kb eb
						    (smartNode ka ea x  y1)
						    (smartNode kc ec y2 z)

-- add the given key-element pair to the map
--
-- overrides previous entries
--
addToFM :: Ord k => k -> e -> FiniteMap k e -> FiniteMap k e
addToFM  = addToCombFM const

-- add the given key-element pair to the map where collisions are resolved by 
-- an explicit combiner fun
--
-- the combiner expects the new element as its first argument
--
addToCombFM :: Ord k
	    => (e -> e -> e) -> k -> e -> FiniteMap k e -> FiniteMap k e
addToCombFM c k e Leaf                 = unitFM k e
addToCombFM c k e (Node k' e' n sm gr) 
	    | k < k'		       = smarterNode k' e' 
						     (addToCombFM c k e sm)
						     gr
	    | k > k'		       = smarterNode k' e' 
						     sm 
						     (addToCombFM c k e gr)
	    | otherwise		       = Node k (c e e') n sm gr

-- removes the key-element pair specified by the given key from a map
--
-- does not complain if the key is not in the map
--
delFromFM                       :: Ord k => k -> FiniteMap k e -> FiniteMap k e
delFromFM k Leaf                 = Leaf
delFromFM k (Node k' e' n sm gr)
	  | k < k'	         = smarterNode k' e' (delFromFM k sm) gr
	  | k > k'		 = smarterNode k' e' sm (delFromFM k gr)
	  | otherwise		 = smartGlue sm gr

-- given two maps where all keys in the left are smaller than those in the
-- right and they are not too far out of balance (within ratio), glue them 
-- into one map
--
smartGlue           :: Ord k => FiniteMap k e -> FiniteMap k e -> FiniteMap k e
smartGlue Leaf gr    = gr
smartGlue sm   Leaf  = sm
smartGlue sm   gr    = let 
		         (k, e, gr') = extractMin gr
		       in
		         smarterNode k e sm gr'

-- extract the association with the minimal key (i.e., leftmost in the tree)
-- and simultaneously return the map without this association
--
extractMin :: Ord k => FiniteMap k e -> (k, e, FiniteMap k e)
extractMin (Node k e _ Leaf gr)  = (k, e, gr)
extractMin (Node k e _ sm   gr)  = let
				     (minK, minE, sm') = extractMin sm
				   in
				     (minK, minE, smarterNode k e sm' gr)

-- given two maps where all keys in the left are smaller than those in the
-- right, glue them into one map
--
glue :: Ord k => FiniteMap k e -> FiniteMap k e -> FiniteMap k e
glue Leaf gr                              = gr
glue sm   Leaf                            = sm
glue sm@(Node k_sm e_sm n_sm sm_sm gr_sm)
     gr@(Node k_gr e_gr n_gr sm_gr gr_gr)
     | (ratio * n_sm) < n_gr
       = smarterNode k_gr e_gr (glue sm sm_gr) gr_gr
     | (ratio * n_gr) < n_sm
       = smarterNode k_sm e_sm sm_sm (glue gr_sm gr)
     | otherwise
       = let 
	   (k, e, gr') = extractMin gr
	 in
	   smarterNode k e sm gr'

-- builds a node that automagically balances the tree if necessary and inserts
-- the right size (just as `smarterNode'), BUT which is only applicable if the
-- two given maps do not overlap (in their key values) and the new, given key
-- lies between the keys in the first and the second map
--
-- its time complexity is proportional to the _difference_ in the height of
-- the two trees representing the given maps
--
smartestNode :: Ord k
	     => k -> e -> (FiniteMap k e) -> (FiniteMap k e) -> (FiniteMap k e)
--
-- if any of both trees is too big (with respect to the ratio), we insert
-- into the other; otherwise, a simple creation of a new node is sufficient
--
smartestNode k e Leaf gr                              = addToFM k e gr
smartestNode k e sm   Leaf                            = addToFM k e sm
smartestNode k e sm@(Node k_sm e_sm n_sm sm_sm gr_sm)
		 gr@(Node k_gr e_gr n_gr sm_gr gr_gr)
		 | (ratio * n_sm) < n_gr
		   = smarterNode k_gr e_gr (smartestNode k e sm sm_gr) gr_gr
		 | (ratio * n_gr) < n_sm
		   = smarterNode k_sm e_sm sm_sm (smartestNode k e gr_sm gr)
		 | otherwise
		   = smartNode k e sm gr

-- joins two maps
--
-- entries in the left map shadow those in the right
--
joinFM :: Ord k => FiniteMap k e -> FiniteMap k e -> FiniteMap k e
--
-- explicitly coded, instead of using `joinCombFM', to avoid the `lookupFM'
-- for each element in the left map, which is unnecessary in this case
--
joinFM m                  Leaf = m
joinFM Leaf               m    = m
joinFM (Node k e _ sm gr) m    = smartestNode k e sm' gr'
				 where
				   sm' = joinFM sm (smaller k m)
				   gr' = joinFM gr (greater k m)

-- joins two maps where collisions are resolved by an explicit combiner fun
--
joinCombFM :: Ord k 
	   => (e -> e -> e) -> FiniteMap k e -> FiniteMap k e -> FiniteMap k e
joinCombFM c m                  Leaf = m
joinCombFM c Leaf               m    = m
joinCombFM c (Node k e _ sm gr) m    = smartestNode k e' sm' gr'
				       where
					 sm' = joinCombFM c sm (smaller k m)
					 gr' = joinCombFM c gr (greater k m)
					 e'  = case lookupFM m k 
					       of
					         Just f  -> c e f
						 Nothing -> e

-- cut the part of the tree that is smaller than the given key out of the
-- map
--
smaller                          :: Ord k 
				 => k -> FiniteMap k e -> FiniteMap k e
smaller _ Leaf		          = Leaf
smaller k (Node k' e _ sm gr) 
	| k < k'		  = smaller k sm
	| k > k'		  = smartestNode k' e sm (smaller k gr)
	| otherwise		  = sm

-- cut the part of the tree that is greater than the given key out of the
-- map
--
greater                          :: Ord k
				 => k -> FiniteMap k e -> FiniteMap k e
greater _ Leaf		          = Leaf
greater k (Node k' e _ sm gr) 
	| k > k'		  = greater k gr
	| k < k'		  = smartestNode k' e (greater k sm) gr
	| otherwise		  = gr

-- given two finite maps, yields a finite map containg all elements of the 
-- first argument except those having a key that is contained in the second
-- map
--
diffFM :: Ord k => FiniteMap k e -> FiniteMap k e' -> FiniteMap k e
diffFM Leaf _                   = Leaf
diffFM m    Leaf                = m
diffFM m    (Node k _ _ sm gr)  = glue (diffFM sm' sm) (diffFM gr' gr)
				  where
				    sm' = smaller k m
				    gr' = greater k m

-- given two finite maps, yield the map containing only entries of which the
-- keys are in both maps
--
-- the elements are taken from the left map
--
intersectFM :: Ord k => FiniteMap k e -> FiniteMap k e -> FiniteMap k e
intersectFM  = intersectCombFM const

-- given two finite maps, yield the map containing only entries of which the
-- keys are in both maps
--
-- the corresponding elements of the two maps are combined using the given,
-- function
--
intersectCombFM :: Ord k
		=> (e -> e -> e) 
		-> FiniteMap k e 
		-> FiniteMap k e 
		-> FiniteMap k e
intersectCombFM c _    Leaf            = Leaf
intersectCombFM c Leaf _               = Leaf
intersectCombFM c (Node k e _ sm gr) m 
		| contained            = smartestNode k (c e e') sm' gr'
		| otherwise            = glue sm' gr'
		where
		  sm'             = intersectCombFM c sm (smaller k m)
		  gr'             = intersectCombFM c gr (greater k m)
		  (contained, e') = case lookupFM m k
				    of
				      Just f  -> (True, f)
				      Nothing -> (False, undefined)

		  undefined = error "FiniteMaps: intersectCombFM: Undefined"

-- given a function on a finite maps elements and a finite map, yield the 
-- finite map where every element is replaced as specified by the function
--
mapFM                      :: Ord k
			   => (k -> e -> e') -> FiniteMap k e -> FiniteMap k e'
mapFM f Leaf                = Leaf
mapFM f (Node k e n sm gr)  = Node k (f k e) n (mapFM f sm) (mapFM f gr)

-- folds a finite map according to a given function and _neutral_ value (with
-- respect to the function) that is used for an empty map
--
foldFM                        :: Ord k
			      => (k -> e -> a -> a) -> a -> FiniteMap k e -> a
foldFM f z Leaf                = z
foldFM f z (Node k e _ sm gr)  = foldFM f (f k e (foldFM f z gr)) sm

-- given a predicate and a finite map, yields the finite map containing all 
-- key-element pairs satisfying the predicate
--
filterFM :: Ord k => (k -> e -> Bool) -> FiniteMap k e -> FiniteMap k e
filterFM p Leaf                           = Leaf
filterFM p (Node k e _ sm gr) | p k e     = smartestNode k e sm' gr'
			      | otherwise = glue sm' gr'
			      where
				sm' = filterFM p sm
				gr' = filterFM p gr

-- given a map and a key, returns `Just e' iff the key associates to `e';
-- if the key is not in the map, `Nothing' is returned
--
lookupFM :: Ord k => FiniteMap k e -> k -> Maybe e
lookupFM Leaf _                          = Nothing
lookupFM (Node k e _ sm gr) k' | k' == k = Just e
			       | k' <  k = lookupFM sm k'
			       | k' >  k = lookupFM gr k'

-- just as `lookupFM', but instead of returning a `Maybe' type, a default
-- value to be returned in case that the key is not in the map has to be 
-- specified
--
lookupDftFM         :: Ord k => FiniteMap k e -> e -> k -> e
lookupDftFM map e k = case lookupFM map k 
		       of
			 Just e' -> e'
			 Nothing -> e

-- given a finite map, yields a list of the key-element pairs
--
toListFM :: Ord k => FiniteMap k e -> [(k, e)]
toListFM  = foldFM (\k e kes -> (k, e):kes) []

-- pretty print routine (used as a method in FiniteMap's instance of `Show')
--
toShowS      :: (Show a, Show b, Ord a) => Int -> FiniteMap a b -> ShowS
toShowS _ fm  = format fm 0
		where
		  format Leaf               _      = id
		  format (Node k e n sm gr) indent = 
		    let
		      this = showString (take indent (repeat ' '))
			     . shows k . showString " --> " . shows e 
			     . showString " (size: " . shows n 
			     . showString ")\n"
		    in
			this 
		      . format sm (indent + 2) 
		      . format gr (indent + 2) 

