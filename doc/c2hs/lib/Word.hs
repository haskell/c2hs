-- (c) The FFI task force, 2001
--
-- Provides fixed sized, unsigned integral types

module Word (
  Word8, Word16, Word32, Word64
) where

-- Constraints applying to all of the following types:
--
-- * For any types, all arithmetic is performed modulo 2^n, where n is the
--   number of bit width of the type.
--
-- * The rules that hold for Enum instances over a bounded type such as Int
--   (see the section of the Haskell report dealing with arithmetic sequences)
--   also hold for the Enum instances over the various Int types defined here.

-- 8 bit natural numbers
--
data Word8 = 0 | 1 | ... | 255
	   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Word8 where ...
instance Real     Word8 where ...
instance Integral Word8 where ...
instance Ix       Word8 where ...

-- 16 bit natural numbers
--
data Word16 = 0 | 1 | ... | 65535
	    deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Word16 where ...
instance Real     Word16 where ...
instance Integral Word16 where ...
instance Ix       Word16 where ...

-- 32 bit natural numbers
--
data Word32 = 0 | 1 | ... | 4294967295
	    deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Word32 where ...
instance Real     Word32 where ...
instance Integral Word32 where ...
instance Ix       Word32 where ...

-- 64 bit natural numbers
--
data Word64 = 0 | 1 | ... | 18446744073709551615
	    deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Word64 where ...
instance Real     Word64 where ...
instance Integral Word64 where ...
instance Ix       Word64 where ...
