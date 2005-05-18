--  Compiler Toolkit: operations on file names
--
--  Author : Manuel M. T. Chakravarty
--  Created: 15 November 98
--
--  Version $Revision: 1.2 $ from $Date: 1999/11/06 14:54:16 $
--
--  Copyright (c) [1998..1999] Manuel M. T. Chakravarty
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
--  Typical operations needed when manipulating file names.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--

module FNameOps (basename, dirname, stripDirname, suffix, stripSuffix, addPath)
where

-- strip directory and suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> libc
--
basename :: FilePath -> FilePath
basename  = stripSuffix . stripDirname   

-- strip basename and suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> ../lib/
--
dirname       :: FilePath -> FilePath
dirname fname  = let
		   slashPoss = [pos | ('/', pos) <- zip fname [0..]]
		 in
		 take (last' (-1) slashPoss + 1) fname

-- remove dirname (EXPORTED)
--
--   eg, ../lib/libc.so -> libc.so
--
stripDirname       :: FilePath -> FilePath
stripDirname fname  = let
			slashPoss = [pos | ('/', pos) <- zip fname [0..]]
		      in
		      drop (last' (-1) slashPoss + 1) fname

-- get suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> .so
--
suffix       :: FilePath -> String
suffix fname  = let
		  dotPoss = [pos | ('.', pos) <- zip fname [0..]]
		in
		drop (last' (length fname) dotPoss) fname

-- remove suffix (EXPORTED)
--
--   eg, ../lib/libc.so -> ../lib/libc
--
stripSuffix       :: FilePath -> FilePath
stripSuffix fname  = let
		       dotPoss = [pos | ('.', pos) <- zip fname [0..]]
		     in
		     take (last' (length fname) dotPoss) fname

-- prepend a path to a file name (EXPORTED)
--
--   eg, ../lib/, libc.so -> ../lib/libc.so
--       ../lib , libc.so -> ../lib/libc.so
--
addPath           :: FilePath -> FilePath -> FilePath
addPath ""   file  = file
addPath path file  = path ++ (if last path == '/' then "" else "/") ++ file


-- auxilliary functions
-- --------------------

-- last' x []            = x
-- last' x [y1, ..., yn] = yn
--
last' :: a -> [a] -> a
last'  = foldl (flip const)
