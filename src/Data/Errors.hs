--  Compiler Toolkit: basic error management
--
--  Author : Manuel M. T. Chakravarty
--  Created: 20 February 95
--
--  Copyright (c) [1995..2000] Manuel M. T. Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This modules exports some auxilliary routines for error handling.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  *  the single lines of error messages shouldn't be to long as file name
--     and position are prepended at each line
--
--- TODO ----------------------------------------------------------------------
--

module Data.Errors (
  -- handling of internal error
  --
  interr, todo,
  --
  -- errors in the compiled program (wrapper to Language.C Error type)
  --
  ErrorLevel(..), Error, makeError, errorLevel, showError, errorAtPos
) where
import Language.C.Data.Error hiding (Error)
import Language.C.Data.Position

type Error = CError

-- internal errors
-- ---------------

-- | raise a fatal internal error; message may have multiple lines
--
interr     :: String -> a
interr msg  = error ("INTERNAL COMPILER ERROR:\n"
                     ++ indentMultilineString 2 msg
                     ++ "\n")

-- | raise a error due to a implementation restriction; message may have multiple
-- lines
--
todo     :: String -> a
todo msg  = error ("Feature not yet implemented:\n"
                   ++ indentMultilineString 2 msg
                   ++ "\n")

-- | produce an 'Error', given its level, position, and a list of lines of
-- the error message that must not be empty
--
makeError :: ErrorLevel -> Position -> [String] -> Error
makeError lvl pos msgs = CError $ ErrorInfo lvl pos msgs


errorAtPos         :: Position -> [String] -> a
errorAtPos pos      = error
                      --FIXME: should be using show here, but Show instance
                      --       for CError from language-c is wierd
                    . showErrorInfo "" . errorInfo
                    . makeError LevelError pos


-- | indent the given multiline text by the given number of spaces
--
indentMultilineString   :: Int -> String -> String
indentMultilineString n  = unlines . (map (spaces++)) . lines
                           where
                             spaces = take n (repeat ' ')
