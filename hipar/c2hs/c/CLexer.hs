--  C -> Haskell Compiler: Lexer for C Header Files
--
--  Author : Manuel M. T. Chakravarty
--  Created: 6 March 99
--
--  Version $Revision: 1.15 $ from $Date: 2001/05/13 11:10:01 $
--
--  Copyright (c) [1999..2000] Manuel M. T. Chakravarty
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
--  Lexer for C header files after being processed by the C preprocessor
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  We assume that the input already went through cpp.  Thus, we do not handle 
--  comments and preprocessor directives here.  The lexer recognizes all tokens
--  of ANCI C except those occuring only in function bodies.  It supports the
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html>.
--
--  Comments:
--
--  * There is no support for the optional feature of extended characters (see
--    K&R A2.5.2) or the corresponding strings (A2.6). 
--
--  * We add `typedef-name' (K&R 8.9) as a token, as proposed in K&R A13.
--    However, as these tokens cannot be recognized lexically, but require a
--    context analysis, they are never produced by the lexer, but instead have 
--    to be introduced in a later phase (by converting the corresponding
--    identifiers). 
--
--  * We also recognize GNU C `__attribute__', `__extension__', `__const', 
--    `__const__', `__restrict', and `__restrict__'.
--
--  * Any line starting with `#pragma' is ignored.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--- TODO ----------------------------------------------------------------------
--
--  * `showsPrec' of `CTokCLit' should produce K&R-conforming escapes;
--    same for `CTokSLit'
--
--  * There are more GNU C specific keywords.  Add them and change `CParser'
--    correspondingly (in particular, most tokens within __attribute ((...))
--    expressions are actually keywords, but we handle them as identifiers at
--    the moment).
--

module CLexer (CToken(..), GnuCTok(..), lexC) 
where 

import Char	 (isDigit)
import List	 ((\\))
import Monad	 (liftM)
import Numeric   (readDec, readOct, readHex)

import Common    (Position, Pos(posOf), incPos, retPos)
import Utils     (Tag(tag))
import Errors    (Error)
import UNames	 (NameSupply, Name, names)
import Idents    (Ident, lexemeToIdent, identToLexeme)
import Lexers    (Regexp, Lexer, Action, epsilon, char, (+>), lexaction,
		  lexmeta, (>|<), (>||<), ctrlLexer, star, plus, quest, alt,
		  string, LexerState, execLexer) 

import C2HSState (CST, raise, getNameSupply) 


-- token definition
-- ----------------

-- possible tokens (EXPORTED)
--
data CToken = CTokLParen   Position		-- `('
	    | CTokRParen   Position		-- `)'
	    | CTokLBracket Position		-- `['
	    | CTokRBracket Position		-- `]'
	    | CTokArrow	   Position		-- `->'
	    | CTokDot	   Position		-- `.'
	    | CTokExclam   Position		-- `!'
	    | CTokTilde	   Position		-- `~'
	    | CTokInc	   Position		-- `++'
	    | CTokDec	   Position		-- `--'
	    | CTokPlus	   Position		-- `+'
	    | CTokMinus	   Position		-- `-'
	    | CTokStar	   Position		-- `*'
	    | CTokSlash	   Position		-- `/'
	    | CTokPercent  Position		-- `%'
	    | CTokAmper	   Position		-- `&'
	    | CTokShiftL   Position		-- `<<'
	    | CTokShiftR   Position		-- `>>'
	    | CTokLess	   Position		-- `<'
	    | CTokLessEq   Position		-- `<='
	    | CTokHigh	   Position		-- `>'
	    | CTokHighEq   Position		-- `>='
	    | CTokEqual	   Position		-- `=='
	    | CTokUnequal  Position		-- `!='
	    | CTokHat	   Position		-- `^'
	    | CTokBar	   Position		-- `|'
	    | CTokAnd	   Position		-- `&&'
	    | CTokOr	   Position		-- `||'
	    | CTokQuest	   Position		-- `?'
	    | CTokColon	   Position		-- `:'
	    | CTokAssign   Position		-- `='
	    | CTokPlusAss  Position		-- `+='
	    | CTokMinusAss Position		-- `-='
	    | CTokStarAss  Position		-- `*='
	    | CTokSlashAss Position		-- `/='
	    | CTokPercAss  Position		-- `%='
	    | CTokAmpAss   Position		-- `&='
	    | CTokHatAss   Position		-- `^='
	    | CTokBarAss   Position		-- `|='
	    | CTokSLAss	   Position		-- `<<='
	    | CTokSRAss	   Position		-- `>>='
	    | CTokComma	   Position		-- `,'
	    | CTokSemic	   Position		-- `;'
	    | CTokLBrace   Position		-- `{'
	    | CTokRBrace   Position		-- `}'
	    | CTokEllipsis Position		-- `...'
	    | CTokChar     Position		-- `char'
	    | CTokConst    Position		-- `const' 
						-- (or `__const', `__const__')
	    | CTokDouble   Position		-- `double'
	    | CTokEnum     Position		-- `enum'
	    | CTokExtern   Position		-- `extern'
	    | CTokFloat    Position		-- `float'
	    | CTokInt      Position		-- `int'
	    | CTokLong     Position		-- `long'
	    | CTokRestrict Position		-- `restrict'
						-- (or `__restrict', 
						-- `__restrict__')
	    | CTokShort    Position		-- `short'
	    | CTokSigned   Position		-- `signed'
	    | CTokSizeof   Position		-- `sizeof'
	    | CTokStatic   Position		-- `static'
	    | CTokStruct   Position		-- `struct'
	    | CTokTypedef  Position		-- `typedef'
	    | CTokUnion    Position		-- `union'
	    | CTokUnsigned Position		-- `unsigned'
	    | CTokVoid     Position		-- `void'
	    | CTokVolatile Position		-- `volatile'
	    | CTokCLit	   Position Char	-- character constant
	    | CTokILit	   Position Integer	-- integer constant
	    | CTokFLit	   Position String	-- float constant
	    | CTokSLit	   Position String	-- string constant (no escapes)
	    | CTokIdent	   Position Ident	-- identifier
	    | CTokTypeName Position Ident	-- `typedef-name' identifier
	    | CTokGnuC     GnuCTok Position	-- special GNU C tokens

-- special tokens used in GNU C extensions to ANSI C
--
data GnuCTok = GnuCAttrTok		-- `__attributes__'
	     | GnuCExtTok		-- `__extension__'
	     deriving (Eq)

instance Pos CToken where
  posOf (CTokLParen   pos  ) = pos
  posOf (CTokRParen   pos  ) = pos
  posOf (CTokLBracket pos  ) = pos
  posOf (CTokRBracket pos  ) = pos
  posOf (CTokArrow    pos  ) = pos
  posOf (CTokDot      pos  ) = pos
  posOf (CTokExclam   pos  ) = pos
  posOf (CTokTilde    pos  ) = pos
  posOf (CTokInc      pos  ) = pos
  posOf (CTokDec      pos  ) = pos
  posOf (CTokPlus     pos  ) = pos
  posOf (CTokMinus    pos  ) = pos
  posOf (CTokStar     pos  ) = pos
  posOf (CTokSlash    pos  ) = pos
  posOf (CTokPercent  pos  ) = pos
  posOf (CTokAmper    pos  ) = pos
  posOf (CTokShiftL   pos  ) = pos
  posOf (CTokShiftR   pos  ) = pos
  posOf (CTokLess     pos  ) = pos
  posOf (CTokLessEq   pos  ) = pos
  posOf (CTokHigh     pos  ) = pos
  posOf (CTokHighEq   pos  ) = pos
  posOf (CTokEqual    pos  ) = pos
  posOf (CTokUnequal  pos  ) = pos
  posOf (CTokHat      pos  ) = pos
  posOf (CTokBar      pos  ) = pos
  posOf (CTokAnd      pos  ) = pos
  posOf (CTokOr	      pos  ) = pos
  posOf (CTokQuest    pos  ) = pos
  posOf (CTokColon    pos  ) = pos
  posOf (CTokAssign   pos  ) = pos
  posOf (CTokPlusAss  pos  ) = pos
  posOf (CTokMinusAss pos  ) = pos
  posOf (CTokStarAss  pos  ) = pos
  posOf (CTokSlashAss pos  ) = pos
  posOf (CTokPercAss  pos  ) = pos
  posOf (CTokAmpAss   pos  ) = pos
  posOf (CTokHatAss   pos  ) = pos
  posOf (CTokBarAss   pos  ) = pos
  posOf (CTokSLAss    pos  ) = pos
  posOf (CTokSRAss    pos  ) = pos
  posOf (CTokComma    pos  ) = pos
  posOf (CTokSemic    pos  ) = pos
  posOf (CTokLBrace   pos  ) = pos
  posOf (CTokRBrace   pos  ) = pos
  posOf (CTokEllipsis pos  ) = pos
  posOf (CTokChar     pos  ) = pos
  posOf (CTokConst    pos  ) = pos
  posOf (CTokDouble   pos  ) = pos
  posOf (CTokEnum     pos  ) = pos
  posOf (CTokExtern   pos  ) = pos
  posOf (CTokFloat    pos  ) = pos
  posOf (CTokInt      pos  ) = pos
  posOf (CTokLong     pos  ) = pos
  posOf (CTokRestrict pos  ) = pos
  posOf (CTokShort    pos  ) = pos
  posOf (CTokSigned   pos  ) = pos
  posOf (CTokSizeof   pos  ) = pos
  posOf (CTokStatic   pos  ) = pos
  posOf (CTokStruct   pos  ) = pos
  posOf (CTokTypedef  pos  ) = pos
  posOf (CTokUnion    pos  ) = pos
  posOf (CTokUnsigned pos  ) = pos
  posOf (CTokVoid     pos  ) = pos
  posOf (CTokVolatile pos  ) = pos
  posOf (CTokCLit     pos _) = pos
  posOf (CTokILit     pos _) = pos
  posOf (CTokFLit     pos _) = pos
  posOf (CTokSLit     pos _) = pos
  posOf (CTokIdent    pos _) = pos
  posOf (CTokTypeName pos _) = pos
  posOf (CTokGnuC   _ pos  ) = pos

instance Show CToken where
  showsPrec _ (CTokLParen   _  ) = showString "("
  showsPrec _ (CTokRParen   _  ) = showString ")"
  showsPrec _ (CTokLBracket _  ) = showString "["
  showsPrec _ (CTokRBracket _  ) = showString "]"
  showsPrec _ (CTokArrow    _  ) = showString "->"
  showsPrec _ (CTokDot	    _  ) = showString "."
  showsPrec _ (CTokExclam   _  ) = showString "!"
  showsPrec _ (CTokTilde    _  ) = showString "~"
  showsPrec _ (CTokInc	    _  ) = showString "++"
  showsPrec _ (CTokDec	    _  ) = showString "--"
  showsPrec _ (CTokPlus	    _  ) = showString "+"
  showsPrec _ (CTokMinus    _  ) = showString "-"
  showsPrec _ (CTokStar	    _  ) = showString "*"
  showsPrec _ (CTokSlash    _  ) = showString "/"
  showsPrec _ (CTokPercent  _  ) = showString "%"
  showsPrec _ (CTokAmper    _  ) = showString "&"
  showsPrec _ (CTokShiftL   _  ) = showString "<<"
  showsPrec _ (CTokShiftR   _  ) = showString ">>"
  showsPrec _ (CTokLess	    _  ) = showString "<"
  showsPrec _ (CTokLessEq   _  ) = showString "<="
  showsPrec _ (CTokHigh	    _  ) = showString ">"
  showsPrec _ (CTokHighEq   _  ) = showString ">="
  showsPrec _ (CTokEqual    _  ) = showString "=="
  showsPrec _ (CTokUnequal  _  ) = showString "!="
  showsPrec _ (CTokHat	    _  ) = showString "^"
  showsPrec _ (CTokBar	    _  ) = showString "|"
  showsPrec _ (CTokAnd	    _  ) = showString "&&"
  showsPrec _ (CTokOr	    _  ) = showString "||"
  showsPrec _ (CTokQuest    _  ) = showString "?"
  showsPrec _ (CTokColon    _  ) = showString ":"
  showsPrec _ (CTokAssign   _  ) = showString "="
  showsPrec _ (CTokPlusAss  _  ) = showString "+="
  showsPrec _ (CTokMinusAss _  ) = showString "-="
  showsPrec _ (CTokStarAss  _  ) = showString "*="
  showsPrec _ (CTokSlashAss _  ) = showString "/="
  showsPrec _ (CTokPercAss  _  ) = showString "%="
  showsPrec _ (CTokAmpAss   _  ) = showString "&="
  showsPrec _ (CTokHatAss   _  ) = showString "^="
  showsPrec _ (CTokBarAss   _  ) = showString "|="
  showsPrec _ (CTokSLAss    _  ) = showString "<<="
  showsPrec _ (CTokSRAss    _  ) = showString ">>="
  showsPrec _ (CTokComma    _  ) = showString ","
  showsPrec _ (CTokSemic    _  ) = showString ";"
  showsPrec _ (CTokLBrace   _  ) = showString "{"
  showsPrec _ (CTokRBrace   _  ) = showString "}"
  showsPrec _ (CTokEllipsis _  ) = showString "..."
  showsPrec _ (CTokChar     _  ) = showString "char"
  showsPrec _ (CTokConst    _  ) = showString "const"
  showsPrec _ (CTokDouble   _  ) = showString "double"
  showsPrec _ (CTokEnum     _  ) = showString "enum"
  showsPrec _ (CTokExtern   _  ) = showString "extern"
  showsPrec _ (CTokFloat    _  ) = showString "float"
  showsPrec _ (CTokInt      _  ) = showString "int"
  showsPrec _ (CTokLong     _  ) = showString "long"
  showsPrec _ (CTokRestrict _  ) = showString "restrict"
  showsPrec _ (CTokShort    _  ) = showString "short"
  showsPrec _ (CTokSigned   _  ) = showString "signed"
  showsPrec _ (CTokSizeof   _  ) = showString "sizeof"
  showsPrec _ (CTokStatic   _  ) = showString "static"
  showsPrec _ (CTokStruct   _  ) = showString "struct"
  showsPrec _ (CTokTypedef  _  ) = showString "typedef"
  showsPrec _ (CTokUnion    _  ) = showString "union"
  showsPrec _ (CTokUnsigned _  ) = showString "unsigned"
  showsPrec _ (CTokVoid     _  ) = showString "void"
  showsPrec _ (CTokVolatile _  ) = showString "volatile"
  showsPrec _ (CTokCLit     _ c) = showChar c
  showsPrec _ (CTokILit     _ i) = (showString . show) i
  showsPrec _ (CTokFLit     _ s) = showString s
  showsPrec _ (CTokSLit     _ s) = showString s
  showsPrec _ (CTokIdent    _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokTypeName _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokGnuC   g _  ) = case g of
				     GnuCAttrTok -> showString "__attribute__"
				     GnuCExtTok  -> showString "__extension__"

instance Tag CToken where
  tag (CTokLParen   _  ) = 1
  tag (CTokRParen   _  ) = 2
  tag (CTokLBracket _  ) = 3
  tag (CTokRBracket _  ) = 4
  tag (CTokArrow    _  ) = 5
  tag (CTokDot      _  ) = 6
  tag (CTokExclam   _  ) = 7
  tag (CTokTilde    _  ) = 8
  tag (CTokInc      _  ) = 9
  tag (CTokDec      _  ) = 10
  tag (CTokPlus     _  ) = 11
  tag (CTokMinus    _  ) = 12
  tag (CTokStar     _  ) = 13
  tag (CTokSlash    _  ) = 14
  tag (CTokPercent  _  ) = 15
  tag (CTokAmper    _  ) = 16
  tag (CTokShiftL   _  ) = 17
  tag (CTokShiftR   _  ) = 18
  tag (CTokLess     _  ) = 19
  tag (CTokLessEq   _  ) = 20
  tag (CTokHigh     _  ) = 21
  tag (CTokHighEq   _  ) = 22
  tag (CTokEqual    _  ) = 23
  tag (CTokUnequal  _  ) = 24
  tag (CTokHat      _  ) = 25
  tag (CTokBar      _  ) = 26
  tag (CTokAnd      _  ) = 27
  tag (CTokOr	    _  ) = 28
  tag (CTokQuest    _  ) = 29
  tag (CTokColon    _  ) = 30
  tag (CTokAssign   _  ) = 31
  tag (CTokPlusAss  _  ) = 32
  tag (CTokMinusAss _  ) = 33
  tag (CTokStarAss  _  ) = 34
  tag (CTokSlashAss _  ) = 35
  tag (CTokPercAss  _  ) = 36
  tag (CTokAmpAss   _  ) = 37
  tag (CTokHatAss   _  ) = 38
  tag (CTokBarAss   _  ) = 39
  tag (CTokSLAss    _  ) = 40
  tag (CTokSRAss    _  ) = 41
  tag (CTokComma    _  ) = 42
  tag (CTokSemic    _  ) = 43
  tag (CTokLBrace   _  ) = 44
  tag (CTokRBrace   _  ) = 45
  tag (CTokEllipsis _  ) = 46
  tag (CTokChar     _  ) = 47
  tag (CTokConst    _  ) = 48
  tag (CTokDouble   _  ) = 49
  tag (CTokEnum     _  ) = 50
  tag (CTokExtern   _  ) = 51
  tag (CTokFloat    _  ) = 52
  tag (CTokInt      _  ) = 53
  tag (CTokLong     _  ) = 54
  tag (CTokRestrict _  ) = 55
  tag (CTokShort    _  ) = 56
  tag (CTokSigned   _  ) = 57
  tag (CTokSizeof   _  ) = 58
  tag (CTokStatic   _  ) = 59
  tag (CTokStruct   _  ) = 60
  tag (CTokTypedef  _  ) = 61
  tag (CTokUnion    _  ) = 62
  tag (CTokUnsigned _  ) = 63
  tag (CTokVoid     _  ) = 64
  tag (CTokVolatile _  ) = 65
  tag (CTokCLit     _ _) = 66
  tag (CTokILit     _ _) = 67
  tag (CTokFLit     _ _) = 68
  tag (CTokSLit     _ _) = 69
  tag (CTokIdent    _ _) = 70
  tag (CTokTypeName _ _) = 71
  tag (CTokGnuC GnuCAttrTok _) = 72
  tag (CTokGnuC GnuCExtTok  _) = 73

-- local state instantiation
-- -------------------------

type CLexState = [Name]
type CLexer    = Lexer  CLexState CToken
type CRegexp   = Regexp CLexState CToken

cLexState :: CST s CLexState
cLexState  = liftM names getNameSupply

-- for actions that need a new unique name
--
infixl 3 `lexactionName`
lexactionName :: CRegexp -> (String -> Position -> Name -> CToken) -> CLexer
re `lexactionName` action = re `lexmeta` action'
  where
    action' str pos (name:ns) = (Just $ Right (action str pos name),
				 incPos pos (length str),
				 ns,
				 Nothing)


-- lexer definition
-- ----------------

-- the lexical definition of the tokens (follows K&R A2.1)
--
--
clexer :: CLexer
clexer  =      identOrKW
	  >||< constant
	  >||< strlit
	  >||< opsep		-- operator or separator
	  >||< whitespace
	  >||< linedir
	  >||< pragma

-- whitespace (follows K&R A2.1) 
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer' 
--
-- * comments are not handled, as we assume the input already went through cpp
--
whitespace :: CLexer
whitespace  =      (char ' ' `lexaction` \_ _ -> Nothing)
	      >||< ctrlLexer

-- #line directive (K&R A12.6)
--
-- * allows further numbers to follow the file name a la GCC
--
linedir :: CLexer
linedir  = char '#' +> ppwhite +> int +> ppwhite +> (fname +> ppwhite)`quest`
	   ((int +> ppwhite +> (int +> ppwhite)`quest`epsilon)`quest` 
	    char '\n')
	   `lexmeta` \str pos ns -> (Nothing, adjustPos str pos, ns, Nothing)
	   where
	     ppwhite = (char ' ' >|< char '\t')`star` epsilon
	     int     = digitNZ +> digit`star` epsilon
	     fname   = char '"' +> infname`star` char '"'
	     --
	     adjustPos str (fname, row, _) = (fname', row', 0)
	       where
	         str'            = dropWhite . drop 1 $ str
		 (rowStr, str'') = span isDigit str'
		 row'		 = read rowStr
		 str'''		 = dropWhite str''
		 fnameStr	 = takeWhile (/= '"') . drop 1 $ str'''
		 fname'		 = if (null str''' || head str''' /= '"')
				   then fname
				   else fnameStr
		 --
		 dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

-- #pragma directive (K&R A12.?)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
pragma :: CLexer
pragma  = char '#' +> ppwhite +> string "pragma" +> anyButNL`star` char '\n'
	  `lexmeta` \_ pos ns -> (Nothing, retPos pos, ns, Nothing)
	  where
	    ppwhite = (char ' ' >|< char '\t')`star` epsilon

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
-- * the following keywords are not recognized as they may only appear in
--   function bodies: auto, break, case, continue, default, do, else, for,
--   goto, if, register, return, switch, and while
--
identOrKW :: CLexer
--
-- the strictness annotations seem to help a bit
--
identOrKW  = 
  letter +> (letter >|< digit)`star` epsilon
  `lexactionName` \cs pos name -> (idkwtok $!pos) cs name
  where
    idkwtok pos "char"          _    = CTokChar     pos
    idkwtok pos "const"		_    = CTokConst    pos
    idkwtok pos "__const"	_    = CTokConst    pos
    idkwtok pos "__const__"	_    = CTokConst    pos
    idkwtok pos "double"	_    = CTokDouble   pos
    idkwtok pos "enum"		_    = CTokEnum     pos
    idkwtok pos "extern"	_    = CTokExtern   pos
    idkwtok pos "float"		_    = CTokFloat    pos
    idkwtok pos "int"		_    = CTokInt      pos
    idkwtok pos "long"		_    = CTokLong     pos
    idkwtok pos "restrict"	_    = CTokRestrict pos
    idkwtok pos "__restrict"	_    = CTokRestrict pos
    idkwtok pos "__restrict__"	_    = CTokRestrict pos
    idkwtok pos "short"		_    = CTokShort    pos
    idkwtok pos "signed"	_    = CTokSigned   pos
    idkwtok pos "sizeof"	_    = CTokSizeof   pos
    idkwtok pos "static"	_    = CTokStatic   pos
    idkwtok pos "struct"	_    = CTokStruct   pos
    idkwtok pos "typedef"	_    = CTokTypedef  pos
    idkwtok pos "union"		_    = CTokUnion    pos
    idkwtok pos "unsigned"	_    = CTokUnsigned pos
    idkwtok pos "void"		_    = CTokVoid     pos
    idkwtok pos "volatile"	_    = CTokVolatile pos
    idkwtok pos "__attribute__"	_    = CTokGnuC     GnuCAttrTok pos
    idkwtok pos "__extension__" _    = CTokGnuC     GnuCExtTok  pos
    idkwtok pos cs              name = CTokIdent    pos 
						    (lexemeToIdent pos cs name)

-- constants (follows K&R A2.5) 
--
-- * K&R explicit mentions `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--
constant :: CLexer
constant  =      intconst
	    >||< charconst
	    >||< floatconst

-- integer constants (follows K&R A2.5.1)
--
intconst :: CLexer
intconst  =      char '0' +> octdigit`star` ul
	         `lexaction` intTok (head . readOct)
	    >||< digitNZ +> digit`star` ul
	         `lexaction` intTok (head . readDec)
	    >||< char '0' +> (char 'x' >|< char 'X') +> hexdigit`star` ul
		 `lexaction` \cs -> intTok (head . readHex) (drop 2 cs)
				    -- drops the `0x' or `0X'
	    where
	      ul = (alt "uUlL" `quest` alt "uUlL")`quest`
		   epsilon

	      intTok reader cs pos = let
				       (i, _) = reader cs  -- drop trailing u/l
				     in
				     Just (CTokILit pos i)

-- character constants (follows K&R A2.5.2)
--
charconst :: CLexer
charconst  =      char '\'' +> (inchar >|< charesc) +> char '\''
	          `lexaction` 
		    \cs pos -> Just (CTokCLit pos ((fst . oneChar) (tail cs)))

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
--
charesc :: CRegexp
charesc  = char '\\' +> (    (alt "ntvbrfa\\?\'\"")
			 >|< (octdigit +> (octdigit`quest`
			      octdigit)`quest` epsilon)
			 >|< (char 'x' +> hexdigit`plus` epsilon)
			)

-- converts the first character denotation of a C-style string to a character
-- and the remaining string
--
oneChar             :: String -> (Char, String)
oneChar ('\\':c:cs)  = case c of
			 'n'  -> ('\n', cs)
			 't'  -> ('\t', cs)
			 'v'  -> ('\v', cs)
			 'b'  -> ('\b', cs)
			 'r'  -> ('\r', cs)
			 'f'  -> ('\f', cs)
			 'a'  -> ('\a', cs)
			 '\\' -> ('\\', cs)
			 '?'  -> ('?', cs)
			 '\'' -> ('\'', cs)
			 '"'  -> ('"', cs)
			 'x'  -> let (i, cs') = (head . readHex) cs
				 in
				 (toEnum i, cs')
			 _    -> let (i, cs') = (head . readOct) (c:cs)
				 in
				 (toEnum i, cs')
oneChar (c   :cs)    = (c, cs)

-- float constants (follows K&R A2.5.3)
--
floatconst :: CLexer
floatconst  = (    mantpart +> exppart`quest` suffix
	       >|< intpart +> exppart +> suffix)
	      `lexaction` \cs pos -> Just (CTokFLit pos cs)
	      where
	        digits    = digit`plus` epsilon
		intpart   = digits
		fractpart = digits
		mantpart  =     intpart`quest` char '.' +> fractpart
			    >|< intpart +> char '.'
		exppart   = (char 'e' >|< char 'E') +> char '-'`quest` digits
	        suffix    = (alt "fFlL")`quest` epsilon

-- string literal (follows K&R A2.6)
--
strlit :: CLexer
strlit  = char '"' +> (instr >|< charesc)`star` char '"'
	  `lexaction` \cs pos -> Just (CTokSLit pos (norm cs))
	  where
	    -- normalize escapes
	    --
	    norm [] = []
	    norm cs = let (c, cs') = oneChar cs
		      in
		      c : norm cs'

opsep :: CLexer
opsep  =      sym "("   CTokLParen
	 >||< sym ")"   CTokRParen
	 >||< sym "["   CTokLBracket
	 >||< sym "]"   CTokRBracket
	 >||< sym "->"  CTokArrow
	 >||< sym "."   CTokDot
	 >||< sym "!"   CTokExclam
	 >||< sym "~"   CTokTilde
	 >||< sym "++"  CTokInc
	 >||< sym "--"  CTokDec
	 >||< sym "+"   CTokPlus
	 >||< sym "-"   CTokMinus
	 >||< sym "*"   CTokStar
	 >||< sym "/"   CTokSlash
	 >||< sym "%"   CTokPercent
	 >||< sym "&"   CTokAmper
	 >||< sym "<<"  CTokShiftL
	 >||< sym ">>"  CTokShiftR
	 >||< sym "<"   CTokLess
	 >||< sym "<="  CTokLessEq
	 >||< sym ">"   CTokHigh
	 >||< sym ">="  CTokHighEq
	 >||< sym "=="  CTokEqual
	 >||< sym "!="  CTokUnequal
	 >||< sym "^"   CTokHat
	 >||< sym "|"   CTokBar
	 >||< sym "&&"  CTokAnd
	 >||< sym "||"  CTokOr
	 >||< sym "?"   CTokQuest
	 >||< sym ":"   CTokColon
	 >||< sym "="   CTokAssign
	 >||< sym "+="  CTokPlusAss
	 >||< sym "-="  CTokMinusAss
	 >||< sym "*="  CTokStarAss
	 >||< sym "/="  CTokSlashAss
	 >||< sym "%="  CTokPercAss
	 >||< sym "&="  CTokAmpAss
	 >||< sym "^="  CTokHatAss
	 >||< sym "|="  CTokBarAss
	 >||< sym "<<=" CTokSLAss
	 >||< sym ">>=" CTokSRAss
	 >||< sym ","   CTokComma
	 >||< sym ";"   CTokSemic
	 >||< sym "{"   CTokLBrace
	 >||< sym "}"   CTokRBrace
	 >||< sym "..." CTokEllipsis
	 where
	   sym cs con = string cs `lexaction` \_ pos -> Just (con pos)

letter, octdigit, digit, hexdigit, inchar, visible :: Regexp s t
letter   = alt ['a'..'z'] >|< alt ['A'..'Z'] >|< char '_'
octdigit = alt ['0'..'7']
digit    = alt ['0'..'9']
digitNZ  = alt ['1'..'9']
hexdigit = alt ['0'..'9'] >|< alt ['a'..'f'] >|< alt ['A'..'F']
inchar   = alt (['\0'..'\255'] \\ ['\\', '\'', '\n', '\f', '\r', '\v'])
instr    = alt (['\0'..'\255'] \\ ['\\', '\"', '\n', '\f', '\r', '\v'])
anyButNL = alt (['\0'..'\255'] \\ ['\n'])
infname  = alt ([' '..'\127'] \\ ['\\', '\'', '\"'])
visible  = alt [' '..'\127']


-- main lexing routine
-- -------------------

-- generate a token sequence out of a string denoting a C header file
-- (EXPORTED) 
--
-- * the given position is attributed to the first character in the string
--
-- * errors are entered into the compiler state
--
lexC        :: String -> Position -> CST s [CToken]
lexC cs pos  = do
	         state <- cLexState
		 let (ts, _, errs) = execLexer clexer (cs, pos, state)
	         mapM raise errs
	         return ts
