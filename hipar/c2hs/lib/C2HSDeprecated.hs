--  C->Haskell Compiler: the old - now deprecated - interface of the
--			 marshaling library
--
--  Author : Manuel M. T. Chakravarty
--  Created: 18 August 1999
--
--  Version $Revision: 1.1 $ from $Date: 2001/02/04 12:27:32 $
--
--  Copyright (c) [2000..2001] Manuel M. T. Chakravarty
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
--  In order to support the continued use of library bindings relying on the
--  marshalling library of the old 0.7.x ("Afterthought") series, we keep the
--  old types and functions alive in this module - at least for a while.
--
--  There is a slight disgression from the original version of the old
--  interface wrt. to `Char' instances of the old conversion classes.
--  However, these have been rarely used and don't make much sense with Unicode
--  `Char's anyway.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module C2HSDeprecated (
  -- the `Addr' module is gone in the New FFI
  Addr, nullAddr, plusAddr, alignAddr, minusAddr,
  -- the names of theses types did change
  CSInt, CLInt, CLLInt, CUSInt, CULInt, CULLInt,
  -- old C2HS-style Storable support
  Storable(sizeof, assign, deref), assignOff, derefOff, assign_, deref_,
  assignOff_, derefOff_,
  -- these have different names now
  mallocBySize, malloc, 
  -- old conversion interface
  cToChar, cFromChar, cToInt, cFromInt, cToFloat, cFromFloat, cToDouble,
  cFromDouble,
  -- old serialisation interface
  ToAddr(stdAddr), FromAddr(addrStdKeep),
  -- exception handling is now part of the standard FFI
  ifRaise, ifNegRaise, ifNegRaise_, ifNullRaise,
  -- old (de)serilisation of lists
  listToAddrWithLen, addrWithLenToList, listToAddrWithMarker,
  addrWithMarkerToList,
  -- marshalling templates
  use, forget, void, ref, toAddr, toAddrKeep, toFromAddr,  
  --
  -- we ex-export all of the new interface that doesn't clash with any of the
  -- old definitions (namespace-wise)
  --
  module C2HS
) where

import Monad (liftM, when)
import C2HS hiding
	     (Storable(..), malloc, void)
import qualified 
       C2HS  (Storable(..), malloc)


-- old FFI material
-- ----------------

-- we did re-export parts of `Addr' in earlier versions; the module is gone
-- in the new FFI, so let's simulate part of it (EXPORTED)
--
type Addr = Ptr ()
nullAddr  = nullPtr
plusAddr  = plusPtr
alignAddr = alignPtr
minusAddr = minusPtr


-- old C2HSConfig material
-- -----------------------

-- old C type names (EXPORTED)
--
type CSInt   = CShort
type CLInt   = CLong
type CLLInt  = CLLong
type CUSInt  = CUShort
type CULInt  = CULong
type CULLInt = CULLong


-- old C2HSBase material
-- ---------------------

-- allocate a raw piece of memory of the given number of bytes in C land
-- (EXPORTED) 
--
mallocBySize :: Int -> IO Addr
mallocBySize  = C2HS.mallocBytes

-- allocate a raw piece of memory in C land that can hold a value of given type
-- (EXPORTED) 
--
malloc :: Storable a => a -> IO Addr
malloc  = mallocBySize . sizeof

-- old C2HS-style `Storable' (EXPORTED)
--
class Storable a where
  sizeof :: a -> Int
  assign :: Addr -> a -> IO Addr
  deref  :: Addr -> IO (a, Addr)

instance Storable Char where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Int8 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Int16 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Int32 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Int64 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Word8 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Word16 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Word32 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Word64 where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Float where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Double where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)
instance Storable Addr where
  sizeof = C2HS.sizeOf
  assign addr v = do
    C2HS.poke (castPtr addr) v
    return (addr `plusPtr` sizeof v)
  deref addr = do
    v <- C2HS.peek (castPtr addr)
    return (v, addr `plusPtr` sizeof v)

-- assignment with byte offset (EXPORTED)
--
assignOff             :: Storable a => Addr -> Int -> a -> IO Addr
assignOff loc off val  = (loc `plusAddr` off) `assign` val

-- dereferencing with byte offset (EXPORTED)
--
derefOff         :: Storable a => Addr -> Int -> IO (a, Addr)
derefOff loc off  = deref (loc `plusAddr` off)

-- as `assign', but discarding the result address (EXPORTED)
--
assign_       :: Storable a => Addr -> a -> IO ()
assign_ adr x  = assign adr x >> return ()

-- as `deref', but discarding the result address (EXPORTED)
--
deref_ :: Storable a => Addr -> IO a
deref_  = liftM fst . deref

-- as `assignOff', but discarding the result address (EXPORTED)
--
assignOff_         :: Storable a => Addr -> Int -> a -> IO ()
assignOff_ loc x off  = assignOff loc x off >> return ()

-- as `derefOff', but discarding the result address (EXPORTED)
--
derefOff_         :: Storable a => Addr -> Int -> IO a
derefOff_ loc off  = liftM fst $ derefOff loc off

-- character conversion, which was dropped due to it not making much sense in
-- the presence of Unicode (EXPORTED)
--
cToChar :: CChar -> Char
cToChar  = castCCharToChar
--
cFromChar :: Char -> CChar
cFromChar  = castCharToCChar

-- old integer conversions (EXPORTED)
--
-- WARNING: This interface is in fact slightly constrained compared to the
--	    original one.  The latter did not require the converted type to be
--	    an instance of `Integral' - in particular, an instance for `Char'
--	    was supported.
--
cToInt   :: Integral a => a -> Int
cToInt    = cIntConv
--
cFromInt :: Integral a => Int -> a
cFromInt  = cIntConv

-- old float conversions (EXPORTED)
--
cToFloat   :: RealFloat a => a -> Float
cToFloat    = cFloatConv
--
cFromFloat :: RealFloat a => Float -> a
cFromFloat  = cFloatConv

-- old double conversions (EXPORTED)
--
cToDouble   :: RealFloat a => a -> Double
cToDouble    = cFloatConv
--
cFromDouble :: RealFloat a => Double -> a
cFromDouble  = cFloatConv

-- cToBool/cFromBool:
--
-- WARNING: The new interface is in slightly constrained compared to the
--	    original one.  The instance `Char' for cToBool/cFromBool is no
--	    longer supported.


-- old C2HSMarsh stuff
-- -------------------

-- old serialisation class (EXPORTED)
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
-- value is returned (EXPORTED)
--
infix 8 `ifNegRaise`
ifNegRaise           :: Integral i => IO i -> String -> IO Int
ifNegRaise op errmsg  = ifRaise cToInt2 (< 0) op (const errmsg)
			where
			  cToInt2 x = let y = cIntConv x
				      in
				      (y, y)

-- same as `ifErrRaise', but discards the return value (EXPORTED)
--
infix 8 `ifNegRaise_`
ifNegRaise_           :: Integral i => IO i -> String -> IO ()
ifNegRaise_ op errmsg  = liftM (const ()) $ ifNegRaise op errmsg

-- executes the given I/O operation and raises an exception if a value of
-- `NULL' is returned (EXPORTED)
-- 
infix 8 `ifNullRaise`
ifNullRaise           :: IO Addr -> String -> IO Addr
ifNullRaise op errmsg  = ifRaise (\x -> (x, x)) (== nullAddr) op (const errmsg)

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


-- old marshalling templates
-- -------------------------

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
