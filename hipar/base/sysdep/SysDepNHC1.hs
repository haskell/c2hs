--  Compiler Toolkit: system dependent stuff (NHC98 1.0pre14)
--
--  Author : Manuel M. T. Chakravarty
--	     with assistance from Malcolm Wallace
--  Derived: 30 November 1999 (from SysDepGHC4.hs)
--
--  Version $Revision: 1.5 $ from $Date: 2001/02/07 09:24:47 $
--
--  Copyright (c) [1996..1999] Manuel M. T. Chakravarty
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
--  NHC98 Haskell Compiler version 1.0
--
--  The original definition of `runPiped' is courtesy of Sven Panne 
--  <Sven.Panne@informatik.uni-muenchen.de> as distributed on the 
--  glasgow-haskell-bugs@dcs.gla.ac.uk mailing list as
--  <3625B8D9.D6873372@informatik.uni-muenchen.de>.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98 (tested with the low-level interfaces of 
--	      nhc98 1.0pre14)
--
--  * may only import `Config'
--
--  * currently no extra options are needed for compilation
--
--  Provided Services
--  -----------------
--
--  * IO monad:
--    - fixpoint combinator
--
--  * Mutable variables and arrays:  CURRENTLY ONLY VARIABLES
--    - creation, read & write
--  
--  * Process management:	     CURRENTLY NOT SUPPORTED FOR NHC
--    - creation of child process connected with pipes
--
--  * Tracing primitive
--
--  * Unsafe integer cells
--
--- TODO ----------------------------------------------------------------------
--

module SysDep (-- for Haskell 1.4/98 compatibility
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
	       unsafeNewIntRef, unsafeReadAndIncIntRef)
where

import Directory   (setCurrentDirectory)
import IO	   (Handle, BufferMode(..), hSetBuffering) 
import Ix          (Ix)
import Monad	   (when)
import NonStdTrace (trace)

import IOExtras    (IORef, newIORef, readIORef, writeIORef, fixIO,
                    IOArray, newIOArray, writeIOArray, readIOArray,
                    boundsIOArray, unsafePerformIO)


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
