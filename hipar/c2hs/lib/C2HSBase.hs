--  C->Haskell Compiler: basic marshaling routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 27 September 99
--
--  Version $Revision: 1.8 $ from $Date: 2000/08/12 10:10:37 $
--
--  Copyright (c) [1999..2000] Manuel M. T. Chakravarty
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
--  This module provides the basic marshaling routines that are used by C
--  interfaces produced with `c2hs'.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--
--  * It should be possible for a binding to choose whether casts from `IntXX' 
--    or `WordXX' types to standard Haskell types are to be bounds checked.
--

module C2HSBase (CharConv(cToChar, cFromChar), IntConv(cToInt, cFromInt),
		 FloatConv(cToFloat, cFromFloat), DoubleConv(cToDouble,
		 cFromDouble), BoolConv(cToBool, cFromBool), cToEnum,
		 cFromEnum,  
		 Storable(sizeof, assign, deref), assignOff, derefOff,
		 assign_, deref_, assignOff_, derefOff_, mallocBySize,
		 malloc, free)  
where

import Char  (ord, chr)
import Monad (when, liftM)

import Int
import Word
import Addr
       hiding (plusAddr)	-- !!!for GHC 4.08
import qualified
       Addr   (plusAddr)	-- !!!for GHC 4.08

import C2HSConfig (CInt, sizeofFloat, sizeofDouble, sizeofAddr)


-- !!!bad hack for GHC 4.08
plusAddr :: Addr -> Int -> Addr
plusAddr a o = Addr.plusAddr a (fromInteger . toInteger $ o)

-- imported C routines
-- -------------------

foreign import ccall "malloc" unsafe
  mallocUnsafe :: CInt -> IO Addr
foreign import ccall "free" unsafe
  free :: Addr -> IO ()


-- conversion classes
-- ------------------

-- `Char' conversion (EXPORTED)
--

class CharConv a where
  cToChar   :: a -> Char
  cFromChar :: Char -> a

instance CharConv Char where
  cToChar   = id
  cFromChar = id

instance CharConv Int8 where
  cToChar   = chr . int8ToInt
  cFromChar = intToInt8 . ord

-- `Int' conversion (EXPORTED)
--

class IntConv a where
  cToInt   :: a -> Int
  cFromInt :: Int -> a

instance IntConv Int64 where
  cToInt   = int64ToInt
  cFromInt = intToInt64

instance IntConv Int32 where
  cToInt   = int32ToInt
  cFromInt = intToInt32

instance IntConv Int16 where
  cToInt   = int16ToInt
  cFromInt = intToInt16

instance IntConv Int8 where
  cToInt   = int8ToInt
  cFromInt = intToInt8

instance IntConv Word64 where
  cToInt   = word64ToInt
  cFromInt = intToWord64

instance IntConv Word32 where
  cToInt   = word32ToInt
  cFromInt = intToWord32

instance IntConv Word16 where
  cToInt   = word16ToInt
  cFromInt = intToWord16

instance IntConv Word8 where
  cToInt   = word8ToInt
  cFromInt = intToWord8

instance IntConv Char where
  cToInt   = ord
  cFromInt = chr

-- `Float' conversion (EXPORTED)
--

class FloatConv a where
  cToFloat   :: a -> Float
  cFromFloat :: Float -> a

instance FloatConv Float where
  cToFloat   = id
  cFromFloat = id

-- `Double' conversion (EXPORTED)
--

class DoubleConv a where
  cToDouble   :: a -> Double
  cFromDouble :: Double -> a

instance DoubleConv Double where
  cToDouble   = id
  cFromDouble = id

-- `Bool' conversion (EXPORTED)
--

class BoolConv a where
  cToBool   :: a -> Bool
  cFromBool :: Bool -> a

instance BoolConv Char where
  cToBool         = (/= '\0')
  cFromBool False = '\0'
  cFromBool True  = '\255'

instance BoolConv Int8 where
  cToBool         = (/= 0)
  cFromBool False = 0
  cFromBool True  = -1

instance BoolConv Int16 where
  cToBool         = (/= 0)
  cFromBool False = 0
  cFromBool True  = -1

instance BoolConv Int32 where
  cToBool         = (/= 0)
  cFromBool False = 0
  cFromBool True  = -1

instance BoolConv Int64 where
  cToBool         = (/= 0)
  cFromBool False = 0
  cFromBool True  = -1

-- enum conversion (EXPORTED)
--

cToEnum :: Enum e => CInt -> e
cToEnum  = toEnum . cToInt

cFromEnum :: Enum e => e -> CInt
cFromEnum  = cFromInt . fromEnum


-- storage management
-- ------------------

-- allocate a raw piece of memory of the given number of bytes in C land
-- (EXPORTED) 
--
mallocBySize   :: Int -> IO Addr
mallocBySize n  = do
		    addr <- mallocUnsafe (cFromInt n)
		    when (addr == nullAddr) $
		      fatal ("Failed to allocate " ++ show n 
			     ++ " byte of memory in C land!")
		    return addr

-- allocate a raw piece of memory in C land that can hold a value of given type
-- (EXPORTED) 
--
malloc :: Storable a => a -> IO Addr
malloc  = mallocBySize . sizeof

-- get rid of offset parameter
--

dropOffWrite :: (Addr -> Int -> a -> IO ()) -> Int -> Addr -> a -> IO Addr
dropOffWrite write size adr x = do
				  write adr 0 x 
				  return $ adr `plusAddr` size

dropOffRead :: (Addr -> Int -> IO a) -> Int -> Addr -> IO (a, Addr)
dropOffRead read size adr = do
			      res <- read adr 0
			      return (res, adr `plusAddr` size)

-- defines access to C land memory (EXPORTED)
--
-- * sizes are in bytes
--
class Storable a where
  sizeof :: a -> Int
  assign :: Addr -> a -> IO Addr
  deref  :: Addr -> IO (a, Addr)

instance Storable Char where
  sizeof = const 1
  assign = dropOffWrite writeCharOffAddr 1
  deref  = dropOffRead readCharOffAddr 1

instance Storable Int8 where
  sizeof = const 1
  assign = dropOffWrite writeInt8OffAddr 1
  deref  = dropOffRead readInt8OffAddr 1

instance Storable Int16 where
  sizeof = const 2
  assign = dropOffWrite writeInt16OffAddr 2
  deref  = dropOffRead readInt16OffAddr 2

instance Storable Int32 where
  sizeof = const 4
  assign = dropOffWrite writeInt32OffAddr 4
  deref  = dropOffRead readInt32OffAddr 4

instance Storable Int64 where
  sizeof = const 8
  assign = dropOffWrite writeInt64OffAddr 8
  deref  = dropOffRead readInt64OffAddr 8

instance Storable Word8 where
  sizeof = const 1
  assign = dropOffWrite writeWord8OffAddr 1
  deref  = dropOffRead readWord8OffAddr 1

instance Storable Word16 where
  sizeof = const 2
  assign = dropOffWrite writeWord16OffAddr 2
  deref  = dropOffRead readWord16OffAddr 2

instance Storable Word32 where
  sizeof = const 4
  assign = dropOffWrite writeWord32OffAddr 4
  deref  = dropOffRead readWord32OffAddr 4

instance Storable Word64 where
  sizeof = const 8
  assign = dropOffWrite writeWord64OffAddr 8
  deref  = dropOffRead readWord64OffAddr 8

instance Storable Float where
  sizeof = const sizeofFloat
  assign = dropOffWrite writeFloatOffAddr sizeofFloat
  deref  = dropOffRead readFloatOffAddr sizeofFloat

instance Storable Double where
  sizeof = const sizeofDouble
  assign = dropOffWrite writeDoubleOffAddr sizeofDouble
  deref  = dropOffRead readDoubleOffAddr sizeofDouble

instance Storable Addr where
  sizeof = const sizeofAddr
  assign = dropOffWrite writeAddrOffAddr sizeofAddr
  deref  = dropOffRead readAddrOffAddr sizeofAddr

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


-- misc routines
-- -------------

-- raise a fatal error
--
fatal     :: String -> a
fatal msg  = error ("C2HSBase: Fatal error: " ++ msg)
