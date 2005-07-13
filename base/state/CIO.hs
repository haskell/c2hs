--  Compiler Toolkit: Compiler I/O 
--
--  Author : Manuel M T Chakravarty
--  Created: 2 November 95
--
--  Version $Revision: 1.29 $ from $Date: 2003/02/12 09:38:35 $
--
--  Copyright (c) [1995...2005] Manuel M T Chakravarty
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
--  This module lifts the Haskell I/O facilities into `STB' and provides some
--  useful extensions.
--
--- DOCU ----------------------------------------------------------------------
--
-- language: Haskell 98
--
--  * the usage of the `...CIO' functions is exactly as that of the 
--    corresponding `...' functions from the Haskell 98 prelude and library
--
--  * error handling can be found in the module `StateTrans' and `State'
--
--  * Also reexports constants, such as `stderr', and data types of `IO' to
--    avoid explicit imports of `IO' in the rest of the compiler.
--
--- TODO ----------------------------------------------------------------------
--

module CIO (-- (verbatim) re-exports
	    --
	    Handle, HandlePosn, IOMode(..), BufferMode(..), SeekMode(..),
	    stdin, stdout, stderr, 
	    isAlreadyExistsError, isDoesNotExistError, isAlreadyInUseError,
	    isFullError, isEOFError, isIllegalOperation, isPermissionError,
	    isUserError, 
	    ioeGetErrorString, ioeGetHandle, ioeGetFileName,
	    --
	    -- file handling
	    --
	    openFileCIO, hCloseCIO,
	    --
	    -- text I/O
	    --
	    putCharCIO, putStrCIO, hPutStrCIO, hPutStrLnCIO, writeFileCIO,
	    readFileCIO, printCIO, getCharCIO, hFlushCIO,  hPutCharCIO,
	    hSetBufferingCIO, hGetBufferingCIO, 
	    newlineCIO, 
	    --
	    -- `Directory'
	    --
	    doesFileExistCIO, removeFileCIO, 
	    --
	    -- `System'
	    --
	    ExitCode(..), exitWithCIO, getArgsCIO, getProgNameCIO, systemCIO,
	    --
	    -- CTK general stuff
	    --
	    fileFindInCIO, mktempCIO)
where

import IO
import Directory
import System

import FileOps	 (fileFindIn, mktemp)
import StateBase (PreCST, liftIO)


-- file handling
-- -------------

openFileCIO     :: FilePath -> IOMode -> PreCST e s Handle
openFileCIO p m  = liftIO (openFile p m)

hCloseCIO   :: Handle -> PreCST e s ()
hCloseCIO h  = liftIO (hClose h)

-- text I/O
-- --------

putCharCIO   :: Char -> PreCST e s ()
putCharCIO c  = liftIO (putChar c)

putStrCIO   :: String -> PreCST e s ()
putStrCIO s  = liftIO (putStr s)

hPutStrCIO     :: Handle -> String -> PreCST e s ()
hPutStrCIO h s  = liftIO (hPutStr h s)

hPutStrLnCIO     :: Handle -> String -> PreCST e s ()
hPutStrLnCIO h s  = liftIO (hPutStrLn h s)

writeFileCIO		    :: FilePath -> String -> PreCST e s ()
writeFileCIO fname contents  = liftIO (writeFile fname contents)

readFileCIO       :: FilePath -> PreCST e s String
readFileCIO fname  = liftIO (readFile fname)

printCIO   :: Show a => a -> PreCST e s ()
printCIO a  = liftIO (print a)

getCharCIO :: PreCST e s Char
getCharCIO  = liftIO getChar

hFlushCIO   :: Handle -> PreCST e s ()
hFlushCIO h  = liftIO (hFlush h)

hPutCharCIO      :: Handle -> Char -> PreCST e s ()
hPutCharCIO h ch  = liftIO (hPutChar h ch)

hSetBufferingCIO     :: Handle  -> BufferMode -> PreCST e s ()
hSetBufferingCIO h m  = liftIO (hSetBuffering h m)

hGetBufferingCIO   :: Handle  -> PreCST e s BufferMode
hGetBufferingCIO h  = liftIO (hGetBuffering h)

-- derived functions
--

newlineCIO :: PreCST e s ()
newlineCIO  = putCharCIO '\n'


-- `Directory'
-- -----------

doesFileExistCIO :: FilePath -> PreCST e s Bool
doesFileExistCIO  = liftIO . doesFileExist

removeFileCIO :: FilePath -> PreCST e s ()
removeFileCIO  = liftIO . removeFile


-- `System'
-- --------

exitWithCIO :: ExitCode -> PreCST e s a
exitWithCIO  = liftIO . exitWith

getArgsCIO :: PreCST e s [String]
getArgsCIO  = liftIO getArgs

getProgNameCIO :: PreCST e s String
getProgNameCIO  = liftIO getProgName

systemCIO :: String -> PreCST e s ExitCode
systemCIO  = liftIO . system


-- general IO routines defined in CTK
-- ----------------------------------

fileFindInCIO            :: FilePath -> [FilePath] -> PreCST e s FilePath
fileFindInCIO file paths  = liftIO $ file `fileFindIn` paths

mktempCIO :: FilePath -> FilePath -> PreCST e s (Handle, FilePath)
mktempCIO pre post = liftIO $ mktemp pre post
