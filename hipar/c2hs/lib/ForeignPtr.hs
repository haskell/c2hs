-- This module defines foreign pointers, i.e. addresses with associated
-- finalizers.  It defined them by way of the legacy foreign objects supported
-- in earlier systems that don't have direct support for foreign pointers yet.

module ForeignPtr ( 
  ForeignPtr,             -- abstract, instance of: Eq
  newForeignPtr,          -- :: Ptr a -> IO () -> IO (ForeignPtr a)
  addForeignPtrFinalizer, -- :: ForeignPtr a -> IO () -> IO ()
  withForeignPtr,         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
  foreignPtrToPtr,	  -- :: ForeignPtr a -> Ptr a
  touchForeignPtr,        -- :: ForeignPtr a -> IO ()
  castForeignPtr	  -- :: ForeignPtr a -> ForeignPtr b
) where

import Ptr  -- we exploit the internal representation of Ptr via Addr
import Addr
import ForeignObj

data ForeignPtr a = ForeignPtr ForeignObj

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr (Ptr addr) m = do
  fobj <- newForeignObj addr m
  return $ (ForeignPtr fobj)

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrFinalizer (ForeignPtr fobj) m =
  addForeignFinalizer fobj m

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr fptr m = do
  res <- m (foreignPtrToPtr fptr)
  touchForeignPtr fptr
  return res

foreignPtrToPtr :: ForeignPtr a -> Ptr a
foreignPtrToPtr (ForeignPtr fobj) = Ptr (foreignObjToAddr fobj)

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr fptr = fptr `seq` return ()

castForeignPtr :: ForeignPtr a -> ForeignPtr b
castForeignPtr (ForeignPtr a) = ForeignPtr a
