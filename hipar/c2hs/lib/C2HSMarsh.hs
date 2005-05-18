--  C->Haskell Compiler: advanced marshaling routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 October 99
--
--  Version $Revision: 1.16 $ from $Date: 2000/08/04 13:16:15 $
--
--  Copyright (c) [1999..2000] Manuel M. T. Chakravarty
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
--  This module provides the advanced marshaling routines that are used by C
--  interfaces produced with `c2hs'.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--
--  * more `marshXY' functions
--

module C2HSMarsh (Marsh(..), marsh1, marsh1_, marsh2, marsh2_, marsh3, marsh3_,
		  ToAddr(stdAddr), FromAddr(addrStdKeep), addrStd, use,
		  forget, void, ref, toAddr, toAddrKeep, toFromAddr,
		  --
		  -- conditional results using `Maybe'
		  --
		  nothingIf, nothingIfNull,
		  --
		  -- exception handling
		  --
		  ifRaise, ifNegRaise, ifNegRaise_, ifNullRaise,
		  --
		  -- (de)serialisation
		  --
		  listToAddrWithLen, addrWithLenToList, listToAddrWithMarker,
		  listToAddrWithMarker)
where

import Monad	(when, liftM, zipWithM_)
import Addr     (Addr, nullAddr)
import Int      (Int8, Int16, Int32)
import Word     (Word8, Word16, Word32)

import C2HSBase (IntConv(cToInt), Storable(sizeof, assign,  deref), deref_,
		 malloc, free) 


-- advanced marshaling support
-- ---------------------------

-- to and from C marshaling of one value (EXPORTED)
--
-- * the first component describes marshaling from Haskell to C (pre
--   marshaling) - NOTE: it already contains the value that is to be marshaled
--
-- * the second component describes marshaling from C to Haskell (post
--   marshaling) 
--
data Marsh a b = (IO b) :> (b -> IO a)

-- family of marshaling wrappers (EXPORTED)
--
-- * the `marshN_' variants discards all post marshaling results
--

marsh1 :: (a' -> IO r) -> (Marsh a a') -> IO (r, a)
marsh1 f (pre :> post) =
  do
    x <- pre
    r <- f x
    x' <- post x
    return (r, x')

marsh1_ :: (a' -> IO r) -> (Marsh a a') -> IO r
marsh1_ f x = liftM fst $ marsh1 f x

marsh2 :: (a' -> b' -> IO r) 
       -> (Marsh a a') -> (Marsh b b') 
       -> IO (r, (a, b))
marsh2 f (pre1 :> post1) (pre2 :> post2) =
  do
    x <- pre1
    y <- pre2
    r <- f x y
    x' <- post1 x
    y' <- post2 y
    return (r, (x', y'))

marsh2_ :: (a' -> b' -> IO c) 
	-> (Marsh a a') -> (Marsh b b') 
	-> IO c
marsh2_ f x y = liftM fst $ marsh2 f x y

marsh3 :: (a' -> b' -> c' -> IO r) 
       -> (Marsh a a') -> (Marsh b b')  -> (Marsh c c') 
       -> IO (r, (a, b, c))
marsh3 f (pre1 :> post1) (pre2 :> post2) (pre3 :> post3) =
  do
    x <- pre1
    y <- pre2
    z <- pre3
    r <- f x y z
    x' <- post1 x
    y' <- post2 y
    z' <- post3 z
    return (r, (x', y', z'))

marsh3_ :: (a' -> b' -> c' -> IO r) 
	-> (Marsh a a') -> (Marsh b b')  -> (Marsh c c') 
	-> IO r
marsh3_ f x y z = liftM fst $ marsh3 f x y z

-- serialisation of Haskell structures into C land memory (EXPORTED)
--

class Storable a => ToAddr a where
  stdAddr   :: a -> IO Addr
  stdAddr x  = do
	         box <- malloc x
		 box `assign` x
		 return box

instance ToAddr Char
instance ToAddr Int8
instance ToAddr Int16
instance ToAddr Int32
instance ToAddr Word8
instance ToAddr Word16
instance ToAddr Word32
instance ToAddr Float
instance ToAddr Double
instance ToAddr Addr

instance ToAddr String where
  stdAddr = listToAddrWithMarker '\NUL'

-- deserialisation of Haskell structures from C land memory (EXPORTED)
--
class Storable a => FromAddr a where
  addrStdKeep :: Addr -> IO a
  addrStdKeep  = deref_

instance FromAddr Char
instance FromAddr Int8
instance FromAddr Int16
instance FromAddr Int32
instance FromAddr Word8
instance FromAddr Word16
instance FromAddr Word32
instance FromAddr Float
instance FromAddr Double
instance FromAddr Addr

instance FromAddr String where
  addrStdKeep = addrWithMarkerToList '\NUL'

-- deserialisation freeing the C land structure (EXPORTED)
--
addrStd     :: FromAddr a => Addr -> IO a
addrStd adr  = do
		 res <- addrStdKeep adr
		 free adr
		 return res

-- copy a value through marshaling (EXPORTED)
--
use :: a -> IO a
use  = return 

-- discard a value during post marshaling (EXPORTED)
--
forget   :: a -> IO ()
forget _  = use ()

-- discard result value (EXPORTED)
--
void :: Monad m => m (a, b) -> m b
void  = liftM snd

-- often used pattern for (in)out parameters (EXPORTED)
--
ref   :: (ToAddr a, FromAddr a) => a -> Marsh a Addr
ref x  = stdAddr x :> addrStd

-- abbreviations used with single argument functions (EXPORTED)
--

toAddr       :: ToAddr v => (Addr -> IO a) -> v -> IO a
toAddr f val  = f `marsh1_` (stdAddr val :> free)

toAddrKeep       :: ToAddr v => (Addr -> IO a) -> v -> IO a
toAddrKeep f val  = f `marsh1_` (stdAddr val :> use)

toFromAddr       :: (ToAddr v, FromAddr v) => (Addr -> IO a) -> v -> IO (a, v)
toFromAddr f val  = f `marsh1` (stdAddr val :> addrStd)


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
nothingIfNull :: (Addr -> a) -> Addr -> Maybe a
nothingIfNull  = nothingIf (== nullAddr)


-- exception handling
-- ------------------

-- executes the given I/O operation and raises an exception if the returned
-- value full fills the given predicate (EXPORTED)
--
-- * the first argument splits the foreign calls result into the part used
--   for error analysis and the part finally returned
--
ifRaise                 :: (a -> (b, c))	-- selector
		        -> (b -> Bool)		-- test for error condition
		        -> IO a			-- foreign call
		        -> (b -> String)	-- produce exception message
		        -> IO c
ifRaise sel p op errfct  = do
			     (errval, res) <- liftM sel op
			     when (p errval) $
			       (ioError . userError) (errfct errval)
			     return res

-- executes the given I/O operation and raises an exception if a negative
-- value is returned
--
infix 8 `ifNegRaise`
ifNegRaise           :: IntConv i => IO i -> String -> IO Int
ifNegRaise op errmsg  = ifRaise cToInt2 (< 0) op (const errmsg)
			where
			  cToInt2 x = let y = cToInt x
				      in
				      (y, y)

-- same as `ifErrRaise', but discards the return value
--
infix 8 `ifNegRaise_`
ifNegRaise_           :: IntConv i => IO i -> String -> IO ()
ifNegRaise_ op errmsg  = liftM (const ()) $ ifNegRaise op errmsg

-- executes the given I/O operation and raises an exception if a value of
-- `NULL' is returned
-- 
infix 8 `ifNullRaise`
ifNullRaise           :: IO Addr -> String -> IO Addr
ifNullRaise op errmsg  = ifRaise (\x -> (x, x)) (== nullAddr) op (const errmsg)


-- (de)serialisation
-- -----------------

-- serialise the given list into newly allocated memory (EXPORTED)
--
-- * structure is stored in `malloc'ed memory (which is not shared) and has to
--   be explicitly `free'd 
--
listToAddrWithLen     :: Storable a => [a] -> IO (Addr, Int)
listToAddrWithLen str  = do
			     mem <- malloc str
			     mem `assign` str
			     return (mem, length str)

-- deserialise the list stored at the given address (EXPORTED)
--
-- * the memory area containing the string is _not_ freed
--
addrWithLenToList          :: Storable a => Addr -> Int -> IO [a]
addrWithLenToList adr len  = liftM fst $ readListFromAddr len adr

-- serialise the given list into newly allocated memory using a special end
-- marker (EXPORTED)
--
-- * structure is stored in `malloc'ed memory (which is not shared) and has to
--   be explicitly `free'd 
--
-- * the generated array is terminated by the end marker, which may not occur
--   in the serialised list
--
listToAddrWithMarker            :: Storable a => a -> [a] -> IO Addr
listToAddrWithMarker marker str  = 
  liftM fst $ listToAddrWithLen (str ++ [marker])

-- deserialise the list stored at the given address and terminated by a
-- special end marker (EXPORTED)
--
-- * the end marker is removed in the Haskell representation
--
-- * the memory area containing the serialised list is _not_ freed
--
addrWithMarkerToList            :: (Storable a, Eq a) => a -> Addr -> IO [a]
addrWithMarkerToList marker adr  = 
  do
    (x, adr') <- deref adr
    if (x == marker) then return []
		     else do
		       xs <- addrWithMarkerToList marker adr'
		       return $ x : xs


-- auxilliary routines
-- -------------------

instance Storable a => Storable [a] where
  sizeof = sum . map sizeof
  assign = writeListToAddr
  deref	 = error "C2HSMarsh: Cannot generically deserialise a list!"

-- serialises a list into the given memory area
--
writeListToAddr           :: Storable a => Addr -> [a] -> IO Addr
writeListToAddr adr []     = return adr
writeListToAddr adr (x:xs) = do
			       adr' <- adr `assign` x
			       writeListToAddr adr' xs

-- extracts a serialised list from the given memory area
--
readListFromAddr       :: Storable a => Int -> Addr -> IO ([a], Addr)
readListFromAddr 0 adr  = return ([], adr)
readListFromAddr n adr  = do
		            (x, adr') <- deref adr
		            (xs, adr'') <- readListFromAddr (n - 1) adr'
		            return (x : xs, adr'')

{-
-- gets a serialised a list from the given memory area
--
-- * an _INFINITE_ list is returned, be sure to stop evaluating it _before_
--   running over the end of the serialised list and be sure to evaluate all
--   of the spine you ever need _before_ the read memory area is deallocated
--
-- * this routine is about as safe as a hand grenade after removing the safety 
--   latch, so handle it with care ;-)
--
readListFromAddr'     :: Storable a => Addr -> IO [(a, Addr)]
readListFromAddr' adr  = 
  do
    (x, adr') <- deref adr
    xs <- unsafePerformIO $ readListFromAddr' adr'
    return $ (x, adr') : xs
 -}