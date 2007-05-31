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
--  * We speed up the equality test between identifiers by assigning an
--    identification number to each of them, and providing a special equality
--    that compares the lexemes only if the identification numbers are equal.
--
--  * The ordering relation on identifiers is also oriented at the
--    identification number and, hence, does *not* follow the alphanumerical
--    ordering of the lexemes of the identifiers. Instead, it provides a fast
--    ordering when identifiers are used as keys in a `FiniteMap'.
--
--  * The ambiguousness resolving number of an identifier is `-1' when no such
--    number is present (so, such identifiers are distinguished from
--    identifiers that share the front part of the lexeme while having an
--    ambiguousness resolving number).
--
--    The ambiguousness resolving number of primitive identifiers (`pid' in the
--    grammar contained in the KCode definition) is `-2' (this gives primitive
--    identifiers a distinct name space).
--
--  * Attributes may be associated to identifiers, except with `OnlyPos'
--    identifiers, which have a position as their only attribute (they do not
--    carry an attribute identifier, which can be used to index attribute
--    tables). 
--
--  * Internal identifiers that are forming a completely unique name space are
--    supported. But note, they do not have a proper lexeme, i.e., they are not
--    suited for code generation.
--
--- TODO ----------------------------------------------------------------------
--
--  * Hashing is not 8bit clean.
--

module Idents (Ident, noARNum, isLegalIdent, lexemeToIdent, internalIdent,
	       onlyPosIdent, cloneIdent, identToLexeme, isIdentSimple,
	       isIdentPrim, stripIdentARNum, getIdentARNum, newIdentARNum,
	       getIdentAttrs, dumpIdent)  
where

import Data.Char
import Position   (Position, Pos(posOf), nopos)
import UNames     (Name)
import Errors     (interr)
import Attributes (Attrs, newAttrsOnlyPos, newAttrs,
		   Attributed(attrsOf), posOfAttrsOf)


-- simple identifier representation (EXPORTED)
--
-- identifiers without an ambiguousness resolving number get `noARNum' as 
-- number
--
data Ident = Ident String	-- lexeme
		   Int		-- ambiguousness resolving number
		   Int		-- id. number to speed up equality check
		   Attrs    	-- attributes of this ident. incl. position

-- the definition of the equality allows identifiers to be equal that are
-- defined at different source text positions, and aims at speeding up the
-- equality test, by comparing the lexemes only if the two numbers are equal
--
instance Eq Ident where
  (Ident s k id _) == (Ident s' k' id' _) =    (k == k') 
					    && (id == id') 
					    && (s == s')

-- this does *not* follow the alphanumerical ordering of the lexemes
--
instance Ord Ident where
  (Ident s k id _) < (Ident s' k' id' _) =    (k < k') 
					   || ((k == k') && (id < id'))
					   || ((k == k') && (id == id')
					       && (s < s'))
  id1 <= id2 = (id1 < id2) || (id1 == id2)

-- for displaying identifiers
--
instance Show Ident where
  showsPrec _ ide = showString ("`" ++ identToLexeme ide ++ "'")

-- identifiers are attributed
--
instance Attributed Ident where
  attrsOf (Ident _ _ _ at) = at

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

-- used as a substitute for the ambiguousness resolving number if it is not 
-- present (EXPORTED)
--
noARNum :: Int
noARNum  = -1

-- used as the ambiguousness resolving number for primitive identifiers
--
primARNum :: Int
primARNum  = -2

-- used as the ambiguousness resolving number for internal identifiers
--
internARNum :: Int
internARNum  = -3

-- checks whether the given lexeme is a legal identifier (EXPORTED)
--
isLegalIdent        :: String -> Bool
isLegalIdent []      = False
isLegalIdent (c:cs)  = if c == '`' then isQualIdent cs 
		       else (isAlpha c || c == '_') && isIdent (c:cs)
		       where
		         isIdent = checkTail . (dropWhile isAlphaNumOrUS)

			 checkTail []        = True
			 checkTail ("##")    = True
			 checkTail ('#':cs') = all isDigit cs'
			 checkTail _	     = False

			 isAlphaNumOrUS c = isAlphaNum c || (c == '_')
			 isAlphaNum	c = isAlpha c || isNum c
			 isAlpha        c = c `elem` ['a'..'z'] ++ ['A'..'Z']
			 isNum          c = c `elem` ['0'..'9']

			 isQualIdent cs = let
					    cs' = skip cs 
					  in 
					    (not . null) cs' 
					    && (checkTail . tail) cs'

			 skip []        = []
			 skip ('\'':cs) = '\'':cs
			 skip ('\\':cs) = case cs of
					    ('\'':cs') -> skip cs'
					    ('\\':cs') -> skip cs'
					    _          -> skip cs
			 skip (c  :cs)  = skip cs

-- given the lexeme of an identifier, yield the abstract identifier (EXPORTED)
--
-- * the only attribute of the resulting identifier is its source text
--   position; as provided in the first argument of this function
--
-- * only minimal error checking, e.g., the characters of the identifier are
--   not checked for being alphanumerical only; the correct lexis of the
--   identifier should be ensured by the caller, e.g., the scanner or
--   `isLegalIdent' 
--
-- * for reasons of simplicity the complete lexeme is hashed (with `quad')
--
lexemeToIdent            :: Position -> String -> Name -> Ident
lexemeToIdent pos l name  = Ident s k (quad s) (newAttrs pos name)
			    where
			      (s, k) = parseIdent pos l

-- generate an internal identifier (has no position and cannot be asccociated
-- with attributes) (EXPORTED)
--
internalIdent   :: String -> Ident
internalIdent s  = Ident s internARNum (quad s) (newAttrsOnlyPos nopos)

-- generate a `only pos' identifier (may not be used to index attribute
-- tables, but has a position value) (EXPORTED)
--
onlyPosIdent       :: Position -> String -> Ident
onlyPosIdent pos l  = Ident s k (quad s) (newAttrsOnlyPos pos)
		      where
			(s, k) = parseIdent pos l

-- Extract the name and ambiguousness resolving number from a lexeme.
--
parseIdent   :: Position -> String -> (String, Int)
parseIdent pos l  
	      = if (null l) 
		then 
		  interr $ "Idents: lexemeToIdent: Empty lexeme! " ++ show pos
		else 
		if (head l == '\'') 
		then
		  parseQuoted (tail l)
		else 
		  parseNorm l
		where
		-- parse lexeme without quotes
		--
		parseNorm []        = ([], noARNum)
		parseNorm ("##")    = ([], primARNum)
		parseNorm ('#':cs)  = ([], ((read . check) cs)::Int)
		parseNorm (c  :cs)  = let
				        (cs', k) = parseNorm cs
				      in
				        (c:cs', k)

		check []       = interr "Idents: lexemeToIdent: Missing\
				        \ number!"
		check ('-':cs) = interr "Idents: lexemeToIdent: Illegal\
				        \ negative number!"
		check s        = s

		-- parse lexeme with quotes
		--
		parseQuoted []        = interr endInQuotes
		parseQuoted ('\\':cs) = parseSpecial cs
		parseQuoted ('\'':cs) = let
					  (rmd, k) = parseNorm cs
					in
					  if (null rmd) then ([], k)
					  else interr afterQuotes
		parseQuoted (c   :cs) = let
					  (cs', k) = parseQuoted cs
					in
					  (c:cs', k)

		endInQuotes  = "Idents: lexemeToIdent: Unexpected end of\
			       \ lexeme (in quotes)!"

		afterQuotes  = "Idents: lexemeToIdent: Superfluous\
			       \ characters after quotes!"

		endInSpecial = "Idents: lexemeToIdent: Unexpected end of\
			       \ lexeme (in escape sequence)!"

		illegalSpecial = "Idents: lexemeToIdent: Illegal escape\
				 \ sequence!"

		-- parse single escaped character, then continue with
		-- `parseQuoted'
		--
		parseSpecial []              = interr endInSpecial
		parseSpecial (c1:c2:c3:cs)
			     | isDigit c1
			       && isDigit c2
			       && isDigit c3 = let
						 (cs', k) = parseQuoted cs
						 ord0     = ord '0'
						 d1       = ord c1 - ord0
						 d2       = ord c2 - ord0
						 d3       = ord c3 - ord0
					       in
						 (chr (100*d1 + 10*d2 + d3)
						  :cs', k)
		parseSpecial (c:cs) 
			     | c == '\\'     = ('\\':cs', k)
			     | c == '\"'     = ('\"':cs', k)
			     | c == '\''     = ('\'':cs', k)
			     | c == 'n'      = ('\n':cs', k)
			     | c == 't'      = ('\t':cs', k)
			     | c == 'r'      = ('\r':cs', k)
					       where
						 (cs', k) = parseQuoted cs
		parseSpecial _               = interr illegalSpecial

-- create an identifier identical to the given one, but with its own set of
-- attributes (EXPORTED)
--
cloneIdent                           :: Ident -> Name -> Ident
cloneIdent (Ident s k idnum at) name  = 
  Ident s k idnum (newAttrs (posOf at) name)

-- given an abstract identifier, yield its lexeme (EXPORTED)
--
identToLexeme		      :: Ident -> String
identToLexeme (Ident s k _ _)  = s ++ suffix
				 where
				   suffix = if      (k == noARNum) 
					    then "" 
					    else if (k == primARNum)
					    then "##"
					    else if (k == internARNum)
					    then "<internal>"
					    else "#" ++ show k

-- test if the given identifier is simple, i.e., has no ambiguousness
-- resolving number and is not a primitive identifier (EXPORTED)
--
isIdentSimple                 :: Ident -> Bool
isIdentSimple (Ident _ k _ _)  = k == noARNum

-- test if the given identifier is a primitive identifier (EXPORTED)
--
isIdentPrim                 :: Ident -> Bool
isIdentPrim (Ident _ k _ _)  = k == primARNum

-- remove ambiguousness resolving of an identifier (EXPORTED)
--
-- NOTE: The new identifier will not be equal (==) to the old one!
--
stripIdentARNum                        :: Ident -> Ident
stripIdentARNum (Ident s k id at) 
  | k == primARNum || k == internARNum  = interr "Idents: stripIdentARNum: \
						 \Not allowed!"
  | otherwise				= Ident s noARNum id at

-- get the ambiguousness resolving of an identifier (EXPORTED)
--
getIdentARNum                        :: Ident -> Int
getIdentARNum (Ident s k id at)
  | k == primARNum || k == internARNum  = interr "Idents: getIdentARNum: \
						 \Not allowed!"
  | otherwise				= k

-- enter a new ambiguousness resolving into the identifier (EXPORTED)
--
-- NOTE: The new identifier will not be equal (==) to the old one!
--
newIdentARNum :: Ident -> Int -> Ident
newIdentARNum (Ident s k id at) k' 
  | k' < 0                              = interr "Idents: newIdentARNum: \
						 \Negative number!"
  | k == primARNum || k == internARNum  = interr "Idents: newIdentARNum: \
						 \Not allowed!"
  | otherwise			        = Ident s k' id at

-- get the attribute identifier associated with the given identifier (EXPORTED)
--
getIdentAttrs                  :: Ident -> Attrs
getIdentAttrs (Ident _ _ _ as)  = as

-- dump the lexeme and its positions into a string for debugging purposes
-- (EXPORTED)
--
dumpIdent     :: Ident -> String
dumpIdent ide  = identToLexeme ide ++ " at " ++ show (posOf ide) 
