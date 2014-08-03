{-# LANGUAGE CPP #-}
--  Compiler Toolkit: Compiler I/O
--
--  Author : Manuel M T Chakravarty
--  Created: 2 November 95
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

module System.CIO (
            --
            -- file handling
            --
            openFile, hClose,
            --
            -- text I/O
            --
            putChar, putStr, putStrLn, hPutStr, hPutStrLn,
            writeFile, readFile, print, getChar, hFlush,
            hPutChar, hSetBuffering, hGetBuffering, newline,
            --
            -- `Directory'
            --
            createDirectoryIfMissing, doesFileExist, removeFile,
            --
            -- `System'
            --
            IO.ExitCode(..), exitWith, getArgs, getProgName, system,
            --
            -- lifting
            --
            liftIO
            )
where

import Prelude (Bool, Char, String, FilePath, (.), ($), Show, return)
import qualified System.IO as IO
import qualified System.Directory   as IO
                  (createDirectoryIfMissing, doesFileExist, removeFile)
import qualified System.Environment as IO (getArgs, getProgName)
import qualified System.Cmd  as IO (system)
import qualified System.Exit as IO (ExitCode(..), exitWith)

import Control.StateBase (PreCST, liftIO)


-- file handling
-- -------------

openFile     :: FilePath -> IO.IOMode -> PreCST e s IO.Handle
openFile p m  = liftIO $ do
  hnd <- IO.openFile p m
#if MIN_VERSION_base(4,2,0)
  --FIXME: really we should be using utf8 for .chs and .hs files
  --       however the current .chs lexer cannot cope with chars
  --       that are over 255, it goes into an infinte loop.
  --       As an workaround, use latin1 encoding for the moment:
  IO.hSetEncoding hnd IO.latin1
#endif
  return hnd

hClose   :: IO.Handle -> PreCST e s ()
hClose h  = liftIO (IO.hClose h)

-- text I/O
-- --------

putChar   :: Char -> PreCST e s ()
putChar c  = liftIO (IO.putChar c)

putStr   :: String -> PreCST e s ()
putStr s  = liftIO (IO.putStr s)

putStrLn   :: String -> PreCST e s ()
putStrLn s  = liftIO (IO.putStrLn s)

hPutStr     :: IO.Handle -> String -> PreCST e s ()
hPutStr h s  = liftIO (IO.hPutStr h s)

hPutStrLn     :: IO.Handle -> String -> PreCST e s ()
hPutStrLn h s  = liftIO (IO.hPutStrLn h s)

writeFile                   :: FilePath -> String -> PreCST e s ()
writeFile fname contents  = do
  --FIXME: see encoding comment with openFile above
  --       this isn't exception-safe
  hnd <- openFile fname IO.WriteMode
  hPutStr hnd contents
  hClose hnd

readFile       :: FilePath -> PreCST e s String
readFile fname  = do
  --FIXME: see encoding comment with openFile above
  hnd <- openFile fname IO.ReadMode
  liftIO (IO.hGetContents hnd)

print   :: Show a => a -> PreCST e s ()
print a  = liftIO (IO.print a)

getChar :: PreCST e s Char
getChar  = liftIO IO.getChar

hFlush   :: IO.Handle -> PreCST e s ()
hFlush h  = liftIO (IO.hFlush h)

hPutChar      :: IO.Handle -> Char -> PreCST e s ()
hPutChar h ch  = liftIO (IO.hPutChar h ch)

hSetBuffering     :: IO.Handle  -> IO.BufferMode -> PreCST e s ()
hSetBuffering h m  = liftIO (IO.hSetBuffering h m)

hGetBuffering   :: IO.Handle  -> PreCST e s IO.BufferMode
hGetBuffering h  = liftIO (IO.hGetBuffering h)

-- derived functions
--

newline :: PreCST e s ()
newline  = putChar '\n'


-- `Directory'
-- -----------

createDirectoryIfMissing   :: Bool -> FilePath -> PreCST e s ()
createDirectoryIfMissing p  = liftIO . IO.createDirectoryIfMissing p

doesFileExist :: FilePath -> PreCST e s Bool
doesFileExist  = liftIO . IO.doesFileExist

removeFile :: FilePath -> PreCST e s ()
removeFile  = liftIO . IO.removeFile


-- `System'
-- --------

exitWith :: IO.ExitCode -> PreCST e s a
exitWith  = liftIO . IO.exitWith

getArgs :: PreCST e s [String]
getArgs  = liftIO IO.getArgs

getProgName :: PreCST e s String
getProgName  = liftIO IO.getProgName

system :: String -> PreCST e s IO.ExitCode
system  = liftIO . IO.system
