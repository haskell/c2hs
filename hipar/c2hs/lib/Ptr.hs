-- This code was taken from `Ptr.lhs' of the fptools repository.
--
-- (c) The FFI task force, 2000
--
-- Parametrised pointer built on top of the legacy `Addr'

module Ptr (
    --------------------------------------------------------------------
    -- Data pointers.
    
    Ptr(..),      -- data Ptr a
    nullPtr,      -- :: Ptr a
    castPtr,      -- :: Ptr a -> Ptr b
    plusPtr,      -- :: Ptr a -> Int -> Ptr b
    alignPtr,     -- :: Ptr a -> Int -> Ptr a
    minusPtr,     -- :: Ptr a -> Ptr b -> Int
    
    --------------------------------------------------------------------
    -- Function pointers.
    
    FunPtr(..),      -- data FunPtr a
    nullFunPtr,      -- :: FunPtr a
    castFunPtr,      -- :: FunPtr a -> FunPtr b
    castFunPtrToPtr, -- :: FunPtr a -> Ptr b
    castPtrToFunPtr, -- :: Ptr a -> FunPtr b
    
    freeHaskellFunPtr, -- :: FunPtr a -> IO ()
    -- Free the function pointer created by foreign export dynamic.

 ) where

import Addr
import IOExts (freeHaskellFunctionPtr)

------------------------------------------------------------------------
-- Data pointers.

newtype Ptr a = Ptr Addr deriving (Eq, Ord)

nullPtr :: Ptr a
nullPtr = Ptr nullAddr

castPtr :: Ptr a -> Ptr b
castPtr (Ptr addr) = Ptr addr

plusPtr :: Ptr a -> Int -> Ptr b
plusPtr (Ptr addr) d = Ptr (addr `plusAddr` (toEnum d))

alignPtr :: Ptr a -> Int -> Ptr a
alignPtr (Ptr addr) a = Ptr (addr `alignAddr` a)

minusPtr :: Ptr a -> Ptr b -> Int
minusPtr (Ptr addr1) (Ptr addr2) = fromEnum (addr1 `minusAddr` addr2)

instance Show (Ptr a) where
  showsPrec p (Ptr addr) = showsPrec p addr


------------------------------------------------------------------------
-- Function pointers for the default calling convention.

newtype FunPtr a = FunPtr Addr deriving (Eq, Ord)

nullFunPtr :: FunPtr a
nullFunPtr = FunPtr nullAddr

castFunPtr :: FunPtr a -> FunPtr b
castFunPtr (FunPtr a) = FunPtr a

castFunPtrToPtr :: FunPtr a -> Ptr b
castFunPtrToPtr (FunPtr a) = Ptr a

castPtrToFunPtr :: Ptr a -> FunPtr b
castPtrToFunPtr (Ptr a) = FunPtr a

freeHaskellFunPtr :: FunPtr a -> IO ()
freeHaskellFunPtr (FunPtr a) = freeHaskellFunctionPtr a

instance Show (FunPtr a) where
  showsPrec p (FunPtr addr) = showsPrec p addr

