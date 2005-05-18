-- (c) The FFI task force, [2000..2001]
--
-- Pointers to external entities that are associated with finalisers, ie,
-- functions that are invoked by the storage manager when the last Haskell
-- reference to the foreign pointer disappears.

module ForeignPtr (
  ForeignPtr,             -- data ForeignPtr a, instances: Eq
  newForeignPtr,          -- :: Ptr a -> IO () -> IO (ForeignPtr a)
  addForeignPtrFinalizer, -- :: ForeignPtr a -> IO () -> IO ()
  withForeignPtr,         -- :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
  foreignPtrToPtr,	  -- :: ForeignPtr a -> Ptr a
  touchForeignPtr,        -- :: ForeignPtr a -> IO ()
  castForeignPtr	  -- :: ForeignPtr a -> ForeignPtr b
) where

import Ptr (Ptr)

-- Pointer to external entity associated with a finalizer
--
-- * concrete implementations may choose a different representation
--
newtype ForeignPtr a = ForeignPtr (Ptr a)
		     deriving (Eq)

-- Create a new foreign pointer using the second argument as a finaliser
--
-- * There is no guarantee on how soon the finaliser is executed after the
--   last reference was dropped; this depends on the details of the Haskell
--   storage manager 
--
-- * The only guarantee given is that the finaliser runs before the program
--   terminates
--
newForeignPtr             :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr p finalizer  = do
  addForeignPtrFinalizer p finalizer
  return $ ForeignPtr p

-- Add another finaliser to a foreign pointer
--
-- * No guarantees are made on the order in which multiple finalisers for a
--   single object are run
--
addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrFinalizer (ForeignPtr fp) finalizer =
  primAddForeignPtrFinalizer fp finalizer

-- This function ensures that the foreign object in question is alive at the
-- given place in the sequence of IO actions
--
-- * This function can be used to express liveness dependencies between
--   ForeignPtrs: for example, if the finaliser for one `ForeignPtr' touches a
--   second ForeignPtr, then it is ensured that the second `ForeignPtr' will
--   stay alive at least as long as the first. This can be useful when you
--   want to manipulate interior pointers to a foreign structure: you can use
--   `touchForeignObj' to express the requirement that the exterior pointer
--   must not be finalised until the interior pointer is no longer referenced.
--
touchForeignPtr                 :: ForeignPtr a -> IO ()
{-# notInline touchForeignPtr #-}
touchForeignPtr (ForeignPtr fo)  =
  fo `seq` return ()

-- Applies an operation to the vanilla pointer associated with a foreign
-- pointer 
--
-- * The foreign object is kept alive at least during the whole action, even
--   if it is not used directly inside. Note that it is not safe to return the
--   pointer from the action and use it after the action completes. All uses
--   of the pointer should be inside the `withForeignPtr' bracket.
--
withForeignPtr                   :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr (ForeignPtr fp) m  = do
  res <- m fp
  touchForeignPtr fp
  return res

-- Extract the vanilla pointer from a foreign pointer
--
-- * This is a potentially dangerous operations, as if the argument to
--   foreignPtrToPtr is the last usage occurence of the given foreign pointer,
--   then its finaliser(s) will be run, which potentially invalidates the
--   plain pointer just obtained. Hence, `touchForeignPtr' must be used
--   wherever it has to be guaranteed that the pointer lives on - i.e., has
--   another usage occurrence. 
--
-- * To avoid subtle coding errors, hand written marshalling code should
--   preferably use `withForeignPtr' rather than combinations of
--   `foreignPtrToPtr' and `touchForeignPtr'. However, the later routines are
--   occasionally preferred in tool generated marshalling code.
--
foreignPtrToPtr                 :: ForeignPtr a -> Ptr a
foreignPtrToPtr (ForeignPtr fp)  = fp

-- Change the type parameter of a pointer
--
castForeignPtr                 :: ForeignPtr a -> ForeignPtr b
castForeignPtr (ForeignPtr fp)  = ForeignPtr fp
