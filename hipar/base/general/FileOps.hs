--  Compiler Toolkit: operations on file
--
--  Author : Manuel M T Chakravarty
--  Created: 6 November 1999
--
--  Version $Revision: 1.3 $ from $Date: 2003/02/12 09:38:34 $
--
--  Copyright (c) [1999..2003] Manuel M T Chakravarty
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

module FileOps (fileFindIn, mktemp)
where

-- standard libs
import Char      (chr, ord)
import Directory (doesFileExist)
import IO	 (Handle, IOMode(..), openFile)
import Monad	 (liftM)
import Random    (newStdGen, randomRs)

import SysDep	 (ioError)
import FNameOps  (dirname, stripDirname, addPath)


-- search for the given file in the given list of directories (EXPORTED)
--
-- * if the file does not exist, an exception is raised
--
-- * if the given file name is absolute, it is first tried whether this file
--   exists, afterwards the path component is stripped and the given
--   directories are searched; otherwise, if the file name is not absolute,
--   the path component is retained while searching the directories
--
fileFindIn              :: FilePath -> [FilePath] -> IO FilePath
""   `fileFindIn` paths  = ioError $ userError "Empty file name"
file `fileFindIn` paths  =
  do
    let (paths', file') = if head file == '/' 
			  then (dirname file : paths, stripDirname file)
			  else (paths, file)
        files  = map (`addPath` file') paths'
    existsFlags <- mapM doesFileExist files
    let existingFiles = [file | (file, flag) <- zip files existsFlags, flag]
    if null existingFiles
      then ioError $ userError (file ++ ": File does not exist")
      else return $ head existingFiles

-- |Create a temporary file with a unique name.
--
-- * A unique sequence of at least six characters and digits is added
--   inbetween the two given components (the latter of which must include the
--   file suffix if any is needed)
--
-- * Default permissions are used, which might not be optimal, but
--   unfortunately the Haskell standard libs don't support proper permission
--   management. 
--
-- * We make 100 attempts on getting a unique filename before giving up.
--
mktemp :: FilePath -> FilePath -> IO (Handle, FilePath)
mktemp pre post =
  do
    rs <- liftM (randomRs (0, 61)) newStdGen
			 -- range for lower and upper case letters plus digits
    createLoop 100 rs
  where
    createLoop 0        _  = ioError . userError $ "mktemp: failed 100 times"
    createLoop attempts rs = let
			       (rs', fname) = nextName rs
			     in do
			       h <- openFile fname ReadWriteMode
			       return (h, fname)
			     `catch` \_ -> createLoop (attempts - 1) rs'
    --
    sixChars :: [Int] -> ([Int], String)
    sixChars is = 
      let
        (sixInts, is') = splitAt 6 is
	--
	toChar i | i < 10    = chr . (ord '0' +)                 $ i
		 | i < 36    = chr . (ord 'A' +) . (subtract 10) $ i
		 | otherwise = chr . (ord 'a' +) . (subtract 36) $ i
	in
	(is', map toChar sixInts)
    --
    nextName :: [Int] -> ([Int], String)
    nextName is = let
		    (is', rndChars) = sixChars is
		  in
		  (is', pre ++ rndChars ++ post)
