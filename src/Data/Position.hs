--  Compiler Toolkit: some basic definitions used all over the place
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 February 95
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
--  This module provides some definitions used throughout all modules of a
--  compiler. 
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * May not import anything apart from `Config'.
--
--- TODO ----------------------------------------------------------------------
--
 
module Data.Position (
  --
  -- source text positions
  --
  Position(Position), Pos (posOf),
  nopos, isNopos,
  dontCarePos,  isDontCarePos,
  builtinPos, isBuiltinPos,
  internalPos, isInternalPos,
  incPos, tabPos, retPos,
) where


-- uniform representation of source file positions; the order of the arguments
-- is important as it leads to the desired ordering of source positions
-- (EXPORTED) 
--
data Position = Position String		-- file name
	{-# UNPACK #-}	 !Int		-- row
	{-# UNPACK #-}	 !Int		-- column
  deriving (Eq, Ord)

instance Show Position where
  show (Position fname row col) = show (fname, row, col)

-- no position (for unknown position information) (EXPORTED)
--
nopos :: Position
nopos  = Position "<no file>" (-1) (-1)

isNopos	:: Position -> Bool
isNopos (Position _ (-1) (-1)) = True
isNopos _                      = False

-- don't care position (to be used for invalid position information) (EXPORTED)
--
dontCarePos :: Position
dontCarePos = Position "<invalid>" (-2) (-2)

isDontCarePos  :: Position -> Bool
isDontCarePos (Position _ (-2) (-2)) = True
isDontCarePos _	                     = False

-- position attached to objects that are hard-coded into the toolkit (EXPORTED)
--
builtinPos :: Position
builtinPos  = Position "<built into the compiler>" (-3) (-3)

isBuiltinPos :: Position -> Bool
isBuiltinPos (Position _ (-3) (-3)) = True
isBuiltinPos _                      = False

-- position used for internal errors (EXPORTED)
--
internalPos :: Position
internalPos = Position "<internal error>" (-4) (-4)

isInternalPos :: Position -> Bool
isInternalPos (Position _ (-4) (-4)) = True
isInternalPos _                      = False

-- instances of the class `Pos' are associated with some source text position
-- don't care position (to be used for invalid position information) (EXPORTED)
--
class Pos a where
  posOf :: a -> Position

-- advance column
--
incPos :: Position -> Int -> Position
incPos (Position fname row col) n = Position fname row (col + n)

-- advance column to next tab positions (tabs are at every 8th column)
--
tabPos :: Position -> Position
tabPos (Position fname row col) =
        Position fname row (col + 8 - (col - 1) `mod` 8)

-- advance to next line
--
retPos :: Position -> Position
retPos (Position fname row col) = Position fname (row + 1) 1
