-- (c) The FFI task force, [2000..2001]
--
-- Provides references to Haskell expressions that is guaranteed not to be
-- affected by garbage collection, ie, it will neither be deallocated nor
-- will the value of the stable pointer itself change during garbage
-- collection (ordinary references may be relocated during garbage
-- collection). Consequently, stable pointers can be passed to foreign code,
-- which can handle it as an opaque reference to a Haskell value. 

module StablePtr (
  StablePtr,          -- data StablePtr a; instances: Eq
  newStablePtr,       -- :: a -> IO (StablePtr a)
  deRefStablePtr,     -- :: StablePtr a -> IO a
  freeStablePtr,      -- :: StablePtr a -> IO ()
  castStablePtrToPtr, -- :: StablePtr a -> Ptr ()
  castPtrToStablePtr  -- :: Ptr () -> StablePtr a
) where

import Ptr (Ptr, castPtr)

-- Pointer to a Haskell expression that is guaranteed not to be affect by
-- garbage collection
--
-- * concrete implementations may choose a different representation
--
-- * In C land, values of type `StablePtr a' are represented by
--
--     typedef void *HsStablePtr;
--
newtype StablePtr a = StablePtr (Ptr a)
		    deriving (Eq)

-- Obtains a stable pointer for an arbitrary Haskell expression
--
newStablePtr   :: a -> IO (StablePtr a)
newStablePtr v  = do
  sp <- primNewStablePtr v
  return $ StablePtr sp

-- Recovers the Haskell value refereed to by a stable pointer
--
deRefStablePtr                :: StablePtr a -> IO a
deRefStablePtr (StablePtr sp)  = do
  primDeRefStablePtr sp

-- Dissolve the association between the stable pointer and the Haskell value
--
-- * Afterwards, if the stable pointer is passed to `deRefStablePtr' or
--   `freeStablePtr', the behaviour is undefined. However, the stable pointer
--   may still be passed to `castStablePtrToPtr', but the `Ptr ()' value
--   returned by `castStablePtrToPtr', in this case, is undefined (in
--   particular, it may be `Ptr.nullPtr'). Nevertheless, the call is guaranteed
--   not to diverge. 
--
freeStablePtr                :: StablePtr a -> IO ()
freeStablePtr (StablePtr sp)  = do
  primFreeStablePtr sp

-- Coerces a stable pointer to vanilla pointer
--
-- * No guarantees are made about the resulting value, except that the
--   original stable pointer can be recovered by `castPtrToStablePtr'. In
--   particular, the address may not refer to a valid memory address and any
--   attempt to pass it to the member functions of the class `Storable' will
--   most likely lead to disaster.
--
-- * A pointer type parametrised with `()' is returned to indicate that no
--   assumption may be made about the entity referenced by the pointer
--
castStablePtrToPtr                :: StablePtr a -> Ptr ()
castStablePtrToPtr (StablePtr sp)  =
  castPtr sp

-- Recover a stable pointer from its associated vanilla pointer value
--
-- * We have
--
--     sp == castPtrToStablePtr (castStablePtrToPtr sp)
--
--   as long as `freeStablePtr' has not been executed on `sp'
--
-- * The behaviour of `castPtrToStablePtr' is undefined for pointers not
--   obtained by `castStablePtrToPtr'
--
castPtrToStablePtr   :: Ptr () -> StablePtr a
castPtrToStablePtr p  =
  castPtr p
