--  Compiler Toolkit: identifiers
--
--  Author : Manuel M. T. Chakravarty
--  Created: 14 February 95
--
--  Copyright (c) [1995..1999] Manuel M. T. Chakravarty
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
--  This module provides an abstract notion of identifiers.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * We speed up the equality test between identifiers by keeping a hash
--
--  * The ordering relation on identifiers is also based on the hash and,
--    hence, does *not* follow the alphanumerical ordering of the lexemes of
--    the identifiers. Instead, it provides a fast ordering when identifiers
--    are used as keys in a `FiniteMap'.
--
--  * Attributes may be associated to identifiers, except with `OnlyPos'
--    identifiers, which have a position as their only attribute (they do not
--    carry an attribute identifier, which can be used to index attribute
--    tables). 
--
--- TODO ----------------------------------------------------------------------
--
--  * Hashing is not 8bit clean.
--

module Idents (Ident, lexemeToIdent, internalIdent,
	       onlyPosIdent, identToLexeme, getIdentAttrs)
where

import Data.Char
import Position   (Position, Pos(posOf), nopos)
import UNames     (Name)
import Errors     (interr)
import Attributes (Attrs, newAttrsOnlyPos, newAttrs,
		   Attributed(attrsOf), posOfAttrsOf)


-- simple identifier representation (EXPORTED)
--
data Ident = Ident String	-- lexeme
 {-# UNBOXED #-}   !Int		-- hash to speed up equality check
		   Attrs    	-- attributes of this ident. incl. position

-- the definition of the equality allows identifiers to be equal that are
-- defined at different source text positions, and aims at speeding up the
-- equality test, by comparing the lexemes only if the two numbers are equal
--
instance Eq Ident where
  (Ident s h _) == (Ident s' h' _) = (h == h') && (s == s')

-- this does *not* follow the alphanumerical ordering of the lexemes
--
instance Ord Ident where
  compare (Ident s h _) (Ident s' h' _) = compare (h, s) (h', s')

-- for displaying identifiers
--
instance Show Ident where
  showsPrec _ ide = showString ("`" ++ identToLexeme ide ++ "'")

-- identifiers are attributed
--
instance Attributed Ident where
  attrsOf (Ident _ _ at) = at

-- identifiers have a canonical position
--
instance Pos Ident where
  posOf = posOfAttrsOf

-- to speed up the equality test we compute some hash-like value for each
-- identifiers lexeme and store it in the identifiers representation

-- hash function from the dragon book pp437; assumes 7 bit characters and needs
-- the (nearly) full range of values guaranteed for `Int' by the Haskell 
-- language definition; can handle 8 bit characters provided we have 29 bit 
-- for the `Int's without sign
--
quad                 :: String -> Int
quad (c1:c2:c3:c4:s)  = ((ord c4 * bits21
			  + ord c3 * bits14 
			  + ord c2 * bits7
			  + ord c1) 
			 `mod` bits28)
			+ (quad s `mod` bits28)
quad (c1:c2:c3:[]  )  = ord c3 * bits14 + ord c2 * bits7 + ord c1
quad (c1:c2:[]     )  = ord c2 * bits7 + ord c1
quad (c1:[]        )  = ord c1
quad ([]           )  = 0

bits7  = 2^7
bits14 = 2^14
bits21 = 2^21
bits28 = 2^28

-- given the lexeme of an identifier, yield the abstract identifier (EXPORTED)
--
-- * the only attribute of the resulting identifier is its source text
--   position; as provided in the first argument of this function
--
-- * only minimal error checking, e.g., the characters of the identifier are
--   not checked for being alphanumerical only; the correct lexis of the
--   identifier should be ensured by the caller, e.g., the scanner.
--
-- * for reasons of simplicity the complete lexeme is hashed (with `quad')
--
lexemeToIdent            :: Position -> String -> Name -> Ident
lexemeToIdent pos s name  = Ident s (quad s) (newAttrs pos name)

-- generate an internal identifier (has no position and cannot be asccociated
-- with attributes) (EXPORTED)
--
internalIdent   :: String -> Ident
internalIdent s  = Ident s (quad s) (newAttrsOnlyPos nopos)

-- generate a `only pos' identifier (may not be used to index attribute
-- tables, but has a position value) (EXPORTED)
--
onlyPosIdent       :: Position -> String -> Ident
onlyPosIdent pos s  = Ident s (quad s) (newAttrsOnlyPos pos)

-- given an abstract identifier, yield its lexeme (EXPORTED)
--
identToLexeme	            :: Ident -> String
identToLexeme (Ident s _ _)  = s

-- get the attribute identifier associated with the given identifier (EXPORTED)
--
getIdentAttrs                  :: Ident -> Attrs
getIdentAttrs (Ident _ _ as)  = as

-- dump the lexeme and its positions into a string for debugging purposes
-- (EXPORTED)
--
dumpIdent     :: Ident -> String
dumpIdent ide  = identToLexeme ide ++ " at " ++ show (posOf ide) 
