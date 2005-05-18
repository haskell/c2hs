--  Compiler Toolkit: system dependent stuff (GHC 3.x version)
--
--  Author : Manuel M. T. Chakravarty
--  Created: 23 July 96
--
--  Version $Revision: 1.15 $ from $Date: 1999/11/30 17:08:22 $
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
--  Glasgow Haskell Compiler (GHC) version 3.x.
--
--  The original definition of `runPiped' is courtesy of Sven Panne 
--  <Sven.Panne@informatik.uni-muenchen.de> as distributed on the 
--  glasgow-haskell-bugs@dcs.gla.ac.uk mailing list as
--  <3625B8D9.D6873372@informatik.uni-muenchen.de>.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 1.4 (tested with the low-level interfaces of GHC 3.02)
--
--  * may only import `Config'
--
--  * has to be compiled with `-fglasgow-exts'
--
--  Provided Services
--  -----------------
--
--  * IO monad:
--    - fixpoint combinator
--
--  * Mutable variables and arrays
--    - creation, read & write
--  
--  * Process management:
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
	       ProcessID,		-- re-exported
	       runPiped,
	       --
	       -- tracing
	       --
	       trace,
	       --
	       -- UNSAFE stuff -- *Real* Haskell Hackers only!!!
	       --
	       unsafeNewIntRef, unsafeReadAndIncIntRef)
where

import Directory  (setCurrentDirectory)
import IO	  (Handle, BufferMode(..), hSetBuffering) 
import Ix         (Ix)
import Monad	  (when)

import PrelGHC    (RealWorld)
import PrelST     (ST)
import PrelArr    (MutableArray, MutableVar,
	           newArray, readArray, writeArray, boundsOfArray,
	           newVar, readVar, writeVar)
import PrelIOBase (IO(..), fixIO, stToIO, unsafePerformIO)
import IOExts	  (trace)
import Posix	  (forkProcess, executeFile,
		   Fd, createPipe, dupTo, fdClose,
		   intToFd, fdToHandle)
import PosixUtil  (ProcessID)


-- Haskell 1.4/98 compatibility
-- ----------------------------

-- renamed in Haskell 98; by exporting this function from this module in all
-- version, we make the rest of the code more portable (EXPORTED)
--
ioError = fail


-- mutable variables and arrays lifted into IO
-- -------------------------------------------

newtype IORef a = MVar (MutableVar RealWorld a)

newIORef   :: a -> IO (IORef a)
newIORef x  = stToIO (newVar x >>= \mv -> return (MVar mv))

writeIORef             :: IORef a -> a -> IO ()
writeIORef (MVar mv) x  = stToIO (writeVar mv x)

readIORef           :: IORef a -> IO a
readIORef (MVar mv)  = stToIO (readVar mv)


newtype Ix i => IOArray i a = MArr (MutableArray RealWorld i a)

newIOArray        :: Ix i => (i, i) -> a -> IO (IOArray i a)
newIOArray bnds x  = stToIO (newArray bnds x >>= \ma -> return (MArr ma))

writeIOArray               :: Ix i => IOArray i a -> i -> a -> IO ()
writeIOArray (MArr ma) i x  = stToIO (writeArray ma i x)

readIOArray             :: Ix i => IOArray i a -> i -> IO a
readIOArray (MArr ma) i  = stToIO (readArray ma i)

boundsIOArray           :: Ix i => IOArray i a -> (i, i)
boundsIOArray (MArr ma)  = boundsOfArray ma


-- Process management
-- ------------------

-- run the given command in a child process whose stdin and stdout are
-- connected with pipes to the current processes (EXPORTED)
--
-- * the command is executed via a `/bin/sh -c' and receives the given
--   arguments and environment
-- * a working directory for the sub process may be specified
-- * the child's PID is returned together with a pipe from the child's stdout
--   and another pipe to the child's stdin
--
-- ATTENTION: At least if the sub-process is also a Haskell program, setting
--	      `LineBuffering' in the code below is not enough.  The
--	      sub-process also has to do that -- although, this seems
--	      necessary only for the direction from the sub-process to the
--	      parent.
--
--	      It is not clear if that is a bug in the following code (or the
--	      library code it uses) or the code of the sub-process.
--
runPiped :: FilePath			    -- command
         -> [String]			    -- arguments
         -> Maybe [(String, String)]	    -- environment
         -> Maybe FilePath		    -- working directory    
         -> IO (ProcessID,Handle,Handle)    -- (child pid, fromChild, toChild)
runPiped path args env dir = 
  do
   (rd1, wd1) <- createPipe
   (rd2, wd2) <- createPipe
   maybePid  <- forkProcess  -- clone yourself
   case maybePid of

      -- child (the clone): execute given command
      --
      Nothing   -> do 
		     maybe (return ()) setCurrentDirectory dir
                     dupTo rd1 (intToFd 0)
                     dupTo wd2 (intToFd 1)
                     mapM_ fdClose [rd1, wd1, rd2, wd2]
                     from <- fdToHandle (intToFd 0)
                     to   <- fdToHandle (intToFd 1)
                     hSetBuffering from LineBuffering
                     hSetBuffering to   LineBuffering
                     executeFile path True args env
                     fail (userError "runPiped (child after exec)")

      -- parent (the original process): set up pipes and return
      --
      Just pid  -> do 
		     mapM_ fdClose [rd1, wd2]
                     fromChild <- fdToHandle rd2
                     toChild   <- fdToHandle wd1
                     hSetBuffering fromChild LineBuffering
                     hSetBuffering toChild   LineBuffering
                     return (pid, fromChild, toChild)


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
unsafeReadAndIncIntRef mv  = unsafePerformIO (
			       do
				 v <- readIORef mv
				 writeIORef mv (v + 1)
				 return v
			     )
