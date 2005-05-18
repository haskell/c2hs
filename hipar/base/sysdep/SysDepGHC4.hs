--  Compiler Toolkit: system dependent stuff (GHC 4.x version; x >= 02)
--
--  Author : Manuel M. T. Chakravarty
--  Derived: 11 March 1999 (from SysDepGHC3.hs)
--
--  Version $Revision: 1.4 $ from $Date: 2000/08/18 05:41:05 $
--
--  Copyright (c) [1996..2000] Manuel M. T. Chakravarty
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
--  This module provides the system dependent routines for building with the
--  Glasgow Haskell Compiler (GHC) version 4.x, from x >= 02.
--
--  The original definition of `runPiped' is courtesy of Sven Panne 
--  <Sven.Panne@informatik.uni-muenchen.de> as distributed on the 
--  glasgow-haskell-bugs@dcs.gla.ac.uk mailing list as
--  <3625B8D9.D6873372@informatik.uni-muenchen.de>.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98 (tested with the low-level interfaces of 
--	      GHC 4.0[2..8])
--
--  * may only import `Config'
--
--  * has to be compiled with `-syslib exts'
--
--  Provided Services
--  -----------------
--
--  * IO monad:
--    - fixpoint combinator
--
--  * Mutable variables and arrays:
--    - creation, read & write
--  
--  * Process management (if provided by `SysDepPosix'):
--    - creation of child process connected with pipes
--
--  * Tracing primitive
--
--  * Unsafe integer cells
--
--  Currently, the functionality of `SysDepPosix' is always available when
--  compiling with GHC, *except* on cygwin.
--
--- TODO ----------------------------------------------------------------------
--

module SysDep (
  --
  -- for Haskell 1.4/98 compatibility
  --
  ioError,
  --
  -- extra IO functions
  --
  fixIO,
  --
  -- mutable variables and arrays (in IO)
  --
  IORef, newIORef, readIORef, writeIORef,
  IOArray, newIOArray, boundsIOArray, readIOArray, writeIOArray, 
  --
  -- fork
  --
  module SysDepPosix,
  --
  -- tracing
  --
  trace,
  --
  -- UNSAFE stuff -- *Real* Haskell Hackers only!!!
  --
  unsafeNewIntRef, unsafeReadAndIncIntRef
) where

import Ix         (Ix)
import Monad	  (when)

import IOExts	  (fixIO, unsafePerformIO,
		   IORef, newIORef, readIORef, writeIORef,
		   IOArray, newIOArray, boundsIOArray, readIOArray,
		   writeIOArray, 
		   trace)

-- other system-dependent components
--
import SysDepPosix


-- UNSAFE mutable variables
-- ------------------------

-- WARNING: The following does not exist, or at least, it belongs to another
--	    world.  And if you believe into the lambda calculus, you don't
--	    want to know about this other world.
--
--		   *** DON'T TOUCH NOR USE THIS STUFF *** 
--              (unless you really know what you are doing!)

-- UNSAFELY create a mutable integer (EXPORTED)
--
unsafeNewIntRef   :: Int -> IORef Int
unsafeNewIntRef i  = unsafePerformIO (newIORef i)

-- UNSAFELY increment a mutable integer and yield its value before the
-- increment (EXPORTED)
--
unsafeReadAndIncIntRef    :: IORef Int -> Int
unsafeReadAndIncIntRef mv  = unsafePerformIO $ do
			       v <- readIORef mv
			       writeIORef mv (v + 1)
			       return v
