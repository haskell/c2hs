--  Compiler Toolkit: posix dependent stuff (GHC 4.x version; x >= 02)
--
--  Author : Manuel M. T. Chakravarty
--  Derived: 18 August 2000 (taken posix stuff from SysDepGHC4.hs)
--
--  Version $Revision: 1.2 $ from $Date: 2003/04/16 11:11:47 $
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
--  This module provides system dependent routines for building with the
--  `posix' package from HSLibs.
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
--  * has to be compiled with `-syslib posix'
--
--  Provided Services
--  -----------------
--
--  * Process management:
--    - creation of child process connected with pipes
--
--- TODO ----------------------------------------------------------------------
--

module SysDepPosix (
  ProcessID,		-- re-exported
  runPiped,
) where

import Directory  (setCurrentDirectory)
import IO	  (Handle, BufferMode(..), hSetBuffering) 

import Posix	 (forkProcess, executeFile,
		  Fd, createPipe, dupTo, fdClose,
		  intToFd, fdToHandle, ProcessID)


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
                     fail "runPiped (child after exec)"

      -- parent (the original process): set up pipes and return
      --
      Just pid  -> do 
		     mapM_ fdClose [rd1, wd2]
                     fromChild <- fdToHandle rd2
                     toChild   <- fdToHandle wd1
                     hSetBuffering fromChild LineBuffering
                     hSetBuffering toChild   LineBuffering
                     return (pid, fromChild, toChild)
