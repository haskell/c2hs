-- (c) The FFI task force, 2001
--
-- Provides fixed sized, signed integral types

module Int (
  Int8, Int16, Int32, Int64
) where

-- Constraints applying to all of the following types:
--
-- * For any types, all arithmetic is performed modulo 2^n, where n is the
--   number of bit width of the type minus one (for the sign).
--
-- * The rules that hold for Enum instances over a bounded type such as Int
--   (see the section of the Haskell report dealing with arithmetic sequences)
--   also hold for the Enum instances over the various Int types defined here.

-- 8 bit integers
--
data Int8 = -128 | ... | -1 | 0 | 1 | ... | 127
	  deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Int8 where ...
instance Real     Int8 where ...
instance Integral Int8 where ...
instance Ix       Int8 where ...

-- 16 bit integers
--
data Int16 = -32768 | ... | -1 | 0 | 1 | ... | 32767
	   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Int16 where ...
instance Real     Int16 where ...
instance Integral Int16 where ...
instance Ix       Int16 where ...

-- 32 bit integers
--
data Int32 = -2147483648 | ... | -1 | 0 | 1 | ... | 2147483647
	   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Int32 where ...
instance Real     Int32 where ...
instance Integral Int32 where ...
instance Ix       Int32 where ...

-- 64 bit integers
--
data Int64 = -9223372036854775808 | ... | -1 | 0 | 1 | ... | 9223372036854775807
	   deriving (Eq, Ord, Enum, Bounded, Show, Read)

instance Num      Int64 where ...
instance Real     Int64 where ...
instance Integral Int64 where ...
instance Ix       Int64 where ...
