-- (c) The FFI task force, [2000..2001]
--
-- Haskell representation of the most important C ISO types not covered in the
-- module `CTypes'

module CTypesISO (
	-- Integral types; instances: Eq, Ord, Num, Read, Show, Enum,
	-- Storable, Bounded, Real, Integral
	--
	CPtrdiff(..), CSize(..), CWchar(..), CSigAtomic(..)

	-- Numeric types; instances: Eq, Ord, Num, Read, Show, Enum,
	-- Storable
	--
	CClock(..),  CTime(..),

        -- Opaque types, instances: Storable
	--
	CFile, CFpos, CJmpBuf
) where


import Int
import Word
import Storable (Storable(..))

-- the exact representations are architecture dependent

newtype CPtrdiff   = CPtrDiff   IntWordXY      -- ptrdiff_t
		   deriving (Eq, Ord, Enum, Bounded, Show, Read)
newtype CSize      = CSize      IntWordXY      -- size_t
		   deriving (Eq, Ord, Enum, Bounded, Show, Read)
newtype CWChar     = CWChar     IntWordXY      -- wchar_t
		   deriving (Eq, Ord, Enum, Bounded, Show, Read)
newtype CSigAtomic = CSigAtomic IntWordXY      -- sig_atomic_t
		   deriving (Eq, Ord, Enum, Bounded, Show, Read)

newtype CClock     = CClock ...                -- clock_t
		   deriving (Eq, Ord, Enum, Show, Read)
newtype CTime	   = CTime  ...                -- time_t
		   deriving (Eq, Ord, Enum, Show, Read)

data CFile         = ...                       -- FILE
data CFpos         = ...                       -- fpos_t
data CJmpBuf       = ...                       -- jmp_buf

instance Num      CPtrDiff   where ...
instance Real     CPtrDiff   where ...
instance Integral CPtrDiff   where ...
instance Num      CSize      where ...
instance Real     CSize      where ...
instance Integral CSize      where ...
instance Num      CWChar     where ...
instance Real     CWChar     where ...
instance Integral CWChar     where ...
instance Num      CSigAtomic where ...
instance Real     CSigAtomic where ...
instance Integral CSigAtomic where ...

instance Num CClock where ...
instance Num CTime  where ...
