-- This code was taken from `MarshalAlloc.lhs' and `PrelMarshalAlloc.lhs' of
-- the fptools repository.
--
-- (c) The FFI task force, 2000
--
-- Marshalling support: basic routines for memory allocation

module MarshalAlloc (
  malloc,       -- :: Storable a =>        IO (Ptr a)
  mallocBytes,  -- ::               Int -> IO (Ptr a)

  alloca,       -- :: Storable a =>        (Ptr a -> IO b) -> IO b
  allocaBytes,  -- ::               Int -> (Ptr a -> IO b) -> IO b

  reallocBytes, -- :: Ptr a -> Int -> IO (Ptr a)

  free          -- :: Ptr a -> IO ()
) where

import Ptr	    (Ptr, nullPtr)
import NewStorable  (Storable(sizeOf))
import CTypesISO    (CSize)

--import PrelIOBase hiding (malloc, _malloc)


-- exported functions
-- ------------------

-- allocate space for storable type
--
malloc :: Storable a => IO (Ptr a)
malloc  = doMalloc undefined
  where
    doMalloc       :: Storable a => a -> IO (Ptr a)
    doMalloc dummy  = mallocBytes (sizeOf dummy)

-- allocate given number of bytes of storage
--
mallocBytes      :: Int -> IO (Ptr a)
mallocBytes size  = failWhenNULL "malloc" (_malloc (fromIntegral size))

-- temporarily allocate space for a storable type
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `alloca f' the allocated storage must
--   not be used after `f' returns
--
alloca :: Storable a => (Ptr a -> IO b) -> IO b
alloca  = doAlloca undefined
  where
    doAlloca       :: Storable a => a -> (Ptr a -> IO b) -> IO b
    doAlloca dummy  = allocaBytes (sizeOf dummy)

-- temporarily allocate the given number of bytes of storage
--
-- * the pointer passed as an argument to the function must *not* escape from
--   this function; in other words, in `allocaBytes n f' the allocated storage
--   must not be used after `f' returns
--
allocaBytes      :: Int -> (Ptr a -> IO b) -> IO b
-- `bracket' is not portable, so this is not exception save for the moment
--allocaBytes size  = bracket (mallocBytes size) free
allocaBytes size m = do
  ptr <- mallocBytes size
  res <- m ptr
  free ptr
  return res

-- adjust a malloc'ed storage area to the given size
--
reallocBytes          :: Ptr a -> Int -> IO (Ptr a)
reallocBytes ptr size  = 
  failWhenNULL "realloc" (_realloc ptr (fromIntegral size))

-- free malloc'ed storage
--
free :: Ptr a -> IO ()
free  = _free


-- auxilliary routines
-- -------------------

-- asserts that the pointer returned from the action in the second argument is
-- non-null
--
failWhenNULL :: String -> IO (Ptr a) -> IO (Ptr a)
failWhenNULL name f = do
   addr <- f
   if addr == nullPtr
--      then ioException (IOError Nothing ResourceExhausted name 
--					"out of memory" Nothing)
      then ioError (userError (name++": out of memory"))
      else return addr

-- basic C routines needed for memory allocation
--
foreign import "malloc"  unsafe _malloc  ::          CSize -> IO (Ptr a)
foreign import "realloc" unsafe _realloc :: Ptr a -> CSize -> IO (Ptr a)
foreign import "free"	 unsafe _free    :: Ptr a -> IO ()
