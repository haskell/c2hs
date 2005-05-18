-- (c) The FFI task force, [2000..2001]
--
-- Provides parametrised data and function pointers

module Ptr (
  -- Data pointers
  --
  Ptr,          -- data Ptr a; instances: Eq, Ord, Show
  nullPtr,      -- :: Ptr a
  castPtr,      -- :: Ptr a -> Ptr b
  plusPtr,      -- :: Ptr a -> Int -> Ptr b
  alignPtr,     -- :: Ptr a -> Int -> Ptr a
  minusPtr,     -- :: Ptr a -> Ptr b -> Int

  -- Function pointers.
  --
  FunPtr,            -- data FunPtr a; instances: Eq, Ord, Show
  nullFunPtr,        -- :: FunPtr a
  castFunPtr,        -- :: FunPtr a -> FunPtr b
  castFunPtrToPtr,   -- :: FunPtr a -> Ptr b
  castPtrToFunPtr,   -- :: Ptr a -> FunPtr b

  freeHaskellFunPtr  -- :: FunPtr a -> IO ()
) where

import Word

-- Parametrised data pointer
--
-- * Suitable type arguments can be used to segregate pointers to incompatible
--   values such that the type checker can spot coding mistakes
--
-- * The size of the value representing a data pointer is system-dependent
--
-- * In C land, values of type `Ptr a' are represented by
--
--     typedef void  *HsPtr;
--
newtype Ptr a = Ptr WordXY
	      deriving (Eq, Ord, Show)

-- Special pointer value that indicates the absence of a data pointer
--
-- * This value should be compatible with the C constant `NULL'
--
nullPtr :: Ptr a
nullPtr  = Ptr primNullPtr

-- Change the type parameter of a pointer
--
castPtr         :: Ptr a -> Ptr b
castPtr (Ptr p)  = Ptr p

-- Advance a pointer by a given number of bytes
--
plusPtr           :: Ptr a -> Int -> Ptr b
plusPtr (Ptr p) d  = Ptr (p `primPlusPtr` d)

-- Align the given pointer at the next higher address boundary that is a
-- multiple of the second argument
--
-- * This operation is idempotent
--
alignPtr           :: Ptr a -> Int -> Ptr a
alignPtr (Ptr p) a  = Ptr (p `primAlignPtr` a)

-- Compute the byte difference between two pointers
--
-- * We have
--
--     p2 == p1 `plusPtr` (p2 `minusPtr` p1)
--
minusPtr                   :: Ptr a -> Ptr b -> Int
minusPtr (Ptr p1) (Ptr p2)  = p1 `primMinusPtr` p2

-- Parametrised function pointer
--
-- * Suitable type arguments can be used to segregate pointers to incompatible
--   values such that the type checker can spot coding mistakes; type
--   arguments should be functionals
--
-- * The size of the value representing a function pointer is system-dependent
--
-- * Data and function pointers may be represented differently on some
--   architectures
--
-- * Routines defined with "foreign export dynamic" should be declared to
--   produce function pointers of the present type
--
-- * In C land, values of type `FunPtr a' are represented by
--
--     typedef void (*HsFunPtr)(void);
--
newtype FunPtr a = FunPtr WordXY
	         deriving (Eq, Ord, Show)

-- Special pointer value that indicates the absence of a function pointer
--
-- * This value should be compatible with the C constant `NULL'
--
nullFunPtr :: FunPtr a
nullPtr     = Ptr primNullFunPtr

-- Change the type parameter of a function pointer
--
castFunPtr            :: FunPtr a -> FunPtr b
castFunPtr (FunPtr p)  = FunPtr p

-- Convert a function into a data pointer
--
castFunPtrToPtr           :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr p)  = Ptr (fromIntegral p)

-- Convert a data into a function pointer
--
castPtrToFunPtr         :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr p)  = FunPtr (fromIntegral p)

-- Deallocate a function pointer obtained via a "foreign export dynamic"
--
freeHaskellFunPtr            :: FunPtr a -> IO ()
freeHaskellFunPtr (FunPtr p)  = primFreeHaskellFunctionPtr p
