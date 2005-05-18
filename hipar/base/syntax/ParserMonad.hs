--  The HiPar Toolkit: General parser monad
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 February 98
--
--  Version $Revision: 1.6 $ from $Date: 2000/03/01 12:36:41 $
--
--  Copyright (c) [1998..2000] Manuel M. T. Chakravarty
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
--  This module provides some basic definitions for building parsers with
--  the generator happy using the `%monad' option.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * The `[Name]' component of the parser monad is threaded (in contrast to
--    the position and input string, which are treated as readers).
--
--- TODO ----------------------------------------------------------------------
--
--  * The fact that `ParseResult' and expecially `Parse' are exported openly
--    and that this exploited in the lexers creates awkward dependencies.
--    Unfortunately, a more abstract export policy might have a negative
--    impact on the performance of the parser (at least without cross-module
--    inlining). 
--

module ParserMonad (ParseResult(..), Parse,
		    thenParse, returnParse, parseError, parseNewName, runParse)
where

import Common (Position)
import UNames (Name)


-- the parser result (EXPORTED)
--
data ParseResult a = PRParsed [Name] a		     -- parsed `a'
	           | PRFailed Position [String]	     -- parse or lex error

-- the monad is a combined reader state-transformer monad with exception
-- handling (EXPORTED)
--
-- * it contains the input string and the current position
--
-- * the result is `PRFailed' in case of an error (i.e., flagging an
--   exception)
--
type Parse a = String -> Position -> [Name] -> ParseResult a

-- standard monad combinators (EXPORTED)
--

thenParse     :: Parse a -> (a -> Parse b) -> Parse b
thenParse p q  = \s pos ns -> case (p s pos ns) of
		                PRParsed ns' t   -> q t s pos ns'
				PRFailed pos msg -> PRFailed pos msg

returnParse   :: a -> Parse a
returnParse t  = \_ _ ns -> PRParsed ns t

-- non-standard combinators
--

-- yield a parse errror at the given position with the given error message
-- (EXPORTED) 
--
parseError     :: [String] -> Parse a
parseError msg  = \_ pos _ -> PRFailed pos msg

-- get a unique name (EXPORTED)
--
parseNewName :: Parse Name
parseNewName  = \_ _ (n:ns) -> PRParsed ns n

-- Apply a given parser to a string with initial position (EXPORTED)
--
-- * A success and a failure continuation are supplied to deal with the
--   parsing result. 
--
runParse        :: (a -> b)				-- success cont
		-> (Position -> [String] -> b)		-- failure cont
		-> [Name]
		-> Position 
		-> String 
		-> (Parse a) 
		-> b
runParse succ fail ns pos s p = 
 case (p s pos ns) of
   PRParsed _ t     -> succ t
   PRFailed pos err -> fail pos err
