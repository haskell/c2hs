--                                                                -*-haskell-*-
--  ** @configure_input@ **
--  ===========================================================================
--  C -> Haskell Compiler: configuration
--
--  Author : Manuel M T Chakravarty
--  Created: 27 September 99
--
--  Copyright (c) [1999..2005] Manuel M T Chakravarty
--
--  This file is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This file is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  Configuration options; largely set by `configure'.
--
--- TODO ----------------------------------------------------------------------
--

module C2HS.Config (
  --
  -- programs and paths
  --
  cpp, cppopts, libfname, hpaths,
  --
  -- parameters of the targeted C compiler
  --
  PlatformSpec(..), defaultPlatformSpec, platformSpecDB
) where

import Foreign  (toBool)
import Foreign.C (CInt(..))
import System.Info (arch, os)

-- program settings
-- ----------------

-- | C preprocessor executable
--
cpp :: FilePath
cpp  = case os of
  "darwin" -> "gcc"
  _        -> "cpp"

-- | C preprocessor options
--
-- * `-x c' forces CPP to regard the input as C code; this option seems to be
--   understood at least on Linux, FreeBSD, and Solaris and seems to make a
--   difference over the default language setting on FreeBSD
--
-- * @-P@ would suppress @#line@ directives
--
cppopts :: [String]
cppopts  = case (os,cpp) of
  -- why is gcc different between all these platforms?
  ("openbsd","cpp") -> ["-xc"]
  (_,"cpp")         -> ["-x", "c"]
  (_,"gcc")         -> ["-E", "-x", "c"]
  _                 -> []

-- | C2HS Library file name
--
libfname :: FilePath
libfname  = "C2HS.hs"

-- | Standard system search paths for header files
--
hpaths :: [FilePath]
hpaths  = [".", "/usr/include", "/usr/local/include"]

-- parameters of the targeted C compiler
-- -------------------------------------

-- | Parameters that characterise implementation-dependent features of the
-- targeted C compiler
--
data PlatformSpec = PlatformSpec {
                      identPS             :: String,  -- platform identifier
                      bitfieldDirectionPS :: Int,     -- to fill bitfields
                      bitfieldPaddingPS   :: Bool,    -- padding or split?
                      bitfieldIntSignedPS :: Bool,    -- `int' signed bitf.?
                      bitfieldAlignmentPS :: Int      -- alignment constraint
                    }

instance Show PlatformSpec where
  show (PlatformSpec ident dir pad intSig align) =
    show ident ++ " <" ++ show dir ++ ", " ++ show pad ++ ", " ++
    show intSig ++ ", " ++ show align ++ ">"

-- | Platform specification for the C compiler used to compile c2hs (which is
-- the default target).
--
defaultPlatformSpec :: PlatformSpec
defaultPlatformSpec = PlatformSpec {
                        identPS             = arch ++ "-" ++ os,
                        bitfieldDirectionPS = bitfieldDirection,
                        bitfieldPaddingPS   = bitfieldPadding,
                        bitfieldIntSignedPS = bitfieldIntSigned,
                        bitfieldAlignmentPS = bitfieldAlignment
                      }

-- | The set of platform specification that may be choosen for cross compiling
-- bindings.
--
platformSpecDB :: [PlatformSpec]
platformSpecDB =
  [
    PlatformSpec {
      identPS             = "x86_64-linux",
      bitfieldDirectionPS = 1,
      bitfieldPaddingPS   = True,
      bitfieldIntSignedPS = True,
      bitfieldAlignmentPS = 1
   },
    PlatformSpec {
      identPS             = "i686-linux",
      bitfieldDirectionPS = 1,
      bitfieldPaddingPS   = True,
      bitfieldIntSignedPS = True,
      bitfieldAlignmentPS = 1
    },
    PlatformSpec {
      identPS             = "m68k-palmos",
      bitfieldDirectionPS = -1,
      bitfieldPaddingPS   = True,
      bitfieldIntSignedPS = True,
      bitfieldAlignmentPS = 1
    }
  ]

-- | indicates in which direction the C compiler fills bitfields
--
-- * the value is 1 or -1, depending on whether the direction is growing
--   towards the MSB
--
bitfieldDirection :: Int
bitfieldDirection  = fromIntegral bitfield_direction

foreign import ccall "config.h" bitfield_direction :: CInt

-- | indicates whether a bitfield that does not fit into a partially filled
-- storage unit in its entirety introduce padding or split over two storage
-- units
--
-- * 'True' means that such a bitfield introduces padding (instead of being
--   split)
--
bitfieldPadding :: Bool
bitfieldPadding  = toBool bitfield_padding

foreign import ccall "config.h" bitfield_padding :: CInt

-- | indicates whether a bitfield of type `int' is signed in the targeted C
-- compiler
--
bitfieldIntSigned :: Bool
bitfieldIntSigned  = toBool bitfield_int_signed

foreign import ccall "config.h" bitfield_int_signed :: CInt

-- | the alignment constraint for a bitfield
--
-- * this makes the assumption that the alignment of a bitfield is independent
--   of the bitfield's size
--
bitfieldAlignment :: Int
bitfieldAlignment  = fromIntegral bitfield_alignment

foreign import ccall "config.h" bitfield_alignment :: CInt
