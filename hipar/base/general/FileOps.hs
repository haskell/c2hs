--  Compiler Toolkit: operations on file
--
--  Author : Manuel M. T. Chakravarty
--  Created: 6 November 1999
--
--  Version $Revision: 1.2 $ from $Date: 1999/11/08 08:17:56 $
--
--  Copyright (c) 19989 Manuel M. T. Chakravarty
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

module FileOps (fileFindIn)
where

import Directory (doesFileExist)

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
