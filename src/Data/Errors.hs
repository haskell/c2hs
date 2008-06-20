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
  -- errors in the compiled program
  --
  ErrorLvl(..), Error, makeError, errorLvl, showError, errorAtPos
) where

import Data.Position (Position(..), isInternalPos)


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


-- errors in the compiled program
-- ------------------------------

-- | the higher the level of an error, the more critical it is
--
data ErrorLvl = WarningErr              -- does not affect compilation
              | ErrorErr                -- cannot generate code
              | FatalErr                -- abort immediately
              deriving (Eq, Ord)

data Error = Error ErrorLvl Position [String]

-- note that the equality to on errors takes into account only the error level
-- and position (not the error text)
--
-- note that these comparisions are expensive (the positions contain the file
-- names as strings)
--
instance Eq Error where
  (Error lvl1 pos1 _) == (Error lvl2 pos2 _) = lvl1 == lvl2 && pos1 == pos2

instance Ord Error where
  (Error lvl1 pos1 _) <  (Error lvl2 pos2 _) = pos1 < pos2
                                               || (pos1 == pos2 && lvl1 < lvl2)
  e1                  <= e2                  = e1 < e2 || e1 == e2


-- | produce an 'Error', given its level, position, and a list of lines of
-- the error message that must not be empty
--
makeError :: ErrorLvl -> Position -> [String] -> Error
makeError  = Error

-- | inquire the error level
--
errorLvl                 :: Error -> ErrorLvl
errorLvl (Error lvl _ _)  = lvl

-- | converts an error into a string using a fixed format
--
-- * the list of lines of the error message must not be empty
--
-- * the format is
--
-- >    <fname>:<row>: (column <col>) [<err lvl>]
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
--
-- * internal errors (identified by a special position value) are formatted as
--
-- >    INTERNAL ERROR!
-- >      >>> <line_1>
-- >      <line_2>
-- >        ...
-- >      <line_n>
--
showError :: Error -> String
showError (Error _   pos               (l:ls))  | isInternalPos pos =
  "INTERNAL ERROR!\n"
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls
showError (Error lvl (Position fname row col) (l:ls))  =
  let
    prefix = fname ++ ":" ++ show (row::Int) ++ ": "
             ++ "(column "
             ++ show (col::Int)
             ++ ") ["
             ++ showErrorLvl lvl
             ++ "] "
    showErrorLvl WarningErr = "WARNING"
    showErrorLvl ErrorErr   = "ERROR"
    showErrorLvl FatalErr   = "FATAL"
  in
  prefix ++ "\n"
  ++ "  >>> " ++ l ++ "\n"
  ++ (indentMultilineString 2 . unlines) ls
showError (Error _  _                  []   )   = interr "Errors: showError:\
                                                        \ Empty error message!"

errorAtPos         :: Position -> [String] -> a
errorAtPos pos msg  = (error . showError . makeError ErrorErr pos) msg

-- | indent the given multiline text by the given number of spaces
--
indentMultilineString   :: Int -> String -> String
indentMultilineString n  = unlines . (map (spaces++)) . lines
                           where
                             spaces = take n (repeat ' ')
