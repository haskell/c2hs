--  C -> Haskell Compiler: Lexer for C Header Files
--
--  Author : Manuel M T Chakravarty, Duncan Coutts
--  Created: 24 May 2005
--
--  Version $Revision: 1.1.2.1 $ from $Date: 2005/06/14 00:16:14 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--  Copyright (c) 2005 Duncan Coutts
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
--  C99 `restrict' extension: <http://www.lysator.liu.se/c/restrict.html> as
--  well as inline functions.
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
--    `__const__', `__inline', `__inline__', `__restrict', and `__restrict__'.
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

{

module CLexer (CToken(..), GnuCTok(..), lexC,
               P, execParser, parseError, getNewName, addTypedef) where 

import Char      (isDigit)
import Numeric   (readDec, readOct, readHex)

import Common    (Position, Pos(posOf))
import Errors    (interr)
import UNames	 (Name)
import Idents    (Ident, lexemeToIdent, identToLexeme)

import Data.Set  (Set)
import qualified Data.Set as Set (mkSet, addToSet, elementOf)

}

$space = [ \ \t ] -- horazontal white space
$eol   = \n

$letter   = [a-zA-Z_]
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = \0-\255 # [ \\ \' \n \f \r \v ]
$instr    = \0-\255 # [ \\ \" \n \f \r \v ]
$anyButNL = \0-\255 # \n
$infname  = \ -\127 # [ \\ \' \" ]
$visible  = \ -\127

@int = $digitNZ$digit*
@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
--
@charesc  = \\([ntvbrfa\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)

-- components of float constants (follows K&R A2.5.3)
--
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits
@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE]\-?@digits
@suffix    = [fFlL]


tokens :-

-- whitespace (follows K&R A2.1) 
--
-- * horizontal and vertical tabs, newlines, and form feeds are filter out by
--   `Lexers.ctrlLexer' 
--
-- * comments are not handled, as we assume the input already went through cpp
--
$white+					;

-- #line directive (K&R A12.6)
--
-- * allows further ints after the file name a la GCC; as the GCC CPP docu
--   doesn't say how many ints there can be, we allow an unbound number
--
\#$space*@int$space*(\"$infname*\"$space*)?(@int$space*)*$eol
  { \pos len str -> setPos (adjustPos (take len str) pos) >> lexToken }

-- #pragma directive (K&R A12.8)
--
-- * we simply ignore any #pragma (but take care to update the position
--   information)
--
\#$space*pragma$anyButNL*$eol		;

-- #itent directive, eg used by rcs/cvs
--
-- * we simply ignore any #itent (but take care to update the position
--   information)
--
\#$space*ident$anyButNL*$eol		;

-- identifiers and keywords (follows K&R A2.3 and A2.4)
--
$letter($letter|$digit)*	{ \pos len str -> idkwtok (take len str) pos }

-- constants (follows K&R A2.5) 
--
-- * K&R explicit mentions `enumeration-constants'; however, as they are
--   lexically identifiers, we do not have an extra case for them
--

-- integer constants (follows K&R A2.5.1)
--
0$octdigit*[uUlL]{0,2}		{ token CTokILit (fst . head . readOct) }
$digitNZ$digit*[uUlL]{0,2}	{ token CTokILit (fst . head . readDec) }
0[xX]$hexdigit*[uUlL]{0,2}	{ token CTokILit (fst . head . readHex . drop 2) }

-- character constants (follows K&R A2.5.2)
--
\'($inchar|@charesc)\'		{ token CTokCLit (fst . oneChar . tail) }

-- float constants (follows K&R A2.5.3)
--
(@mantpart@exppart?|@intpart@exppart)@suffix?	{ token CTokFLit id }

-- string literal (follows K&R A2.6)
--
\"($instr|@charesc)*\"			{ token CTokSLit normalizeEscapes }


-- operators and separators
--
"("	{ token_ CTokLParen }
")"	{ token_ CTokRParen  }
"["	{ token_ CTokLBracket }
"]"	{ token_ CTokRBracket }
"->"	{ token_ CTokArrow }
"."	{ token_ CTokDot }
"!"	{ token_ CTokExclam }
"~"	{ token_ CTokTilde }
"++"	{ token_ CTokInc }
"--"	{ token_ CTokDec }
"+"	{ token_ CTokPlus }
"-"	{ token_ CTokMinus }
"*"	{ token_ CTokStar }
"/"	{ token_ CTokSlash }
"%"	{ token_ CTokPercent }
"&"	{ token_ CTokAmper }
"<<"	{ token_ CTokShiftL }
">>"	{ token_ CTokShiftR }
"<"	{ token_ CTokLess }
"<="	{ token_ CTokLessEq }
">"	{ token_ CTokHigh }
">="	{ token_ CTokHighEq }
"=="	{ token_ CTokEqual }
"!="	{ token_ CTokUnequal }
"^"	{ token_ CTokHat }
"|"	{ token_ CTokBar }
"&&"	{ token_ CTokAnd }
"||"	{ token_ CTokOr }
"?"	{ token_ CTokQuest }
":"	{ token_ CTokColon }
"="	{ token_ CTokAssign }
"+="	{ token_ CTokPlusAss }
"-="	{ token_ CTokMinusAss }
"*="	{ token_ CTokStarAss }
"/="	{ token_ CTokSlashAss }
"%="	{ token_ CTokPercAss }
"&="	{ token_ CTokAmpAss }
"^="	{ token_ CTokHatAss }
"|="	{ token_ CTokBarAss }
"<<="	{ token_ CTokSLAss }
">>="	{ token_ CTokSRAss }
","	{ token_ CTokComma }
\;	{ token_ CTokSemic }
"{"	{ token_ CTokLBrace }
"}"	{ token_ CTokRBrace }
"..."	{ token_ CTokEllipsis }


{

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
	    | CTokRBrace   Position		--
	    | CTokEllipsis Position		-- `...'
	    | CTokAlignof  Position		-- `alignof' 
						-- (or `__alignof', 
						-- `__alignof__')
	    | CTokAsm      Position		-- `asm'
	    					-- (or `__asm',
						-- `__asm__')
	    | CTokAuto     Position		-- `auto'
	    | CTokBreak    Position		-- `break'
	    | CTokCase     Position		-- `case'
	    | CTokChar     Position		-- `char'
	    | CTokConst    Position		-- `const' 
						-- (or `__const', `__const__')
	    | CTokContinue Position		-- `continue' 
	    | CTokDefault  Position		-- `default'
	    | CTokDo       Position		-- `do'
	    | CTokDouble   Position		-- `double'
	    | CTokElse     Position		-- `else'
	    | CTokEnum     Position		-- `enum'
	    | CTokExtern   Position		-- `extern'
 	    | CTokFloat    Position		-- `float'
 	    | CTokFor      Position		-- `for'
 	    | CTokGoto     Position		-- `goto'
 	    | CTokIf       Position		-- `if'
	    | CTokInline   Position		-- `inline'
						-- (or `__inline', 
						-- `__inline__')
	    | CTokInt      Position		-- `int'
	    | CTokLong     Position		-- `long'
	    | CTokRegister Position		-- `register'
	    | CTokRestrict Position		-- `restrict'
						-- (or `__restrict', 
						-- `__restrict__')
	    | CTokReturn   Position		-- `return'
	    | CTokShort    Position		-- `short'
	    | CTokSigned   Position		-- `signed'
						-- (or `__signed', 
						-- `__signed__')
	    | CTokSizeof   Position		-- `sizeof'
	    | CTokStatic   Position		-- `static'
	    | CTokStruct   Position		-- `struct'
	    | CTokSwitch   Position		-- `switch'
	    | CTokTypedef  Position		-- `typedef'
	    | CTokUnion    Position		-- `union'
	    | CTokUnsigned Position		-- `unsigned'
	    | CTokVoid     Position		-- `void'
	    | CTokVolatile Position		-- `volatile'
						-- (or `__volatile', 
						-- `__volatile__')
	    | CTokWhile    Position		-- `while'
	    | CTokCLit	   Position Char	-- character constant
	    | CTokILit	   Position Integer	-- integer constant
	    | CTokFLit	   Position String	-- float constant
	    | CTokSLit	   Position String	-- string constant (no escapes)
	    | CTokIdent	   Position Ident	-- identifier

	      -- not generated here, but in `CParser.parseCHeader'
	    | CTokTyIdent  Position Ident	-- `typedef-name' identifier
	    | CTokGnuC GnuCTok Position		-- special GNU C tokens
	    | CTokEof				-- end of file

-- special tokens used in GNU C extensions to ANSI C
--
data GnuCTok = GnuCAttrTok		-- `__attribute__'
	     | GnuCExtTok		-- `__extension__'

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
  posOf (CTokAlignof  pos  ) = pos
  posOf (CTokAsm      pos  ) = pos
  posOf (CTokAuto     pos  ) = pos
  posOf (CTokBreak    pos  ) = pos
  posOf (CTokCase     pos  ) = pos
  posOf (CTokChar     pos  ) = pos
  posOf (CTokConst    pos  ) = pos
  posOf (CTokContinue pos  ) = pos
  posOf (CTokDefault  pos  ) = pos
  posOf (CTokDo       pos  ) = pos
  posOf (CTokDouble   pos  ) = pos
  posOf (CTokElse     pos  ) = pos
  posOf (CTokEnum     pos  ) = pos
  posOf (CTokExtern   pos  ) = pos
  posOf (CTokFloat    pos  ) = pos
  posOf (CTokFor      pos  ) = pos
  posOf (CTokGoto     pos  ) = pos
  posOf (CTokInt      pos  ) = pos
  posOf (CTokInline   pos  ) = pos
  posOf (CTokIf       pos  ) = pos
  posOf (CTokLong     pos  ) = pos
  posOf (CTokRegister pos  ) = pos
  posOf (CTokRestrict pos  ) = pos
  posOf (CTokReturn   pos  ) = pos
  posOf (CTokShort    pos  ) = pos
  posOf (CTokSigned   pos  ) = pos
  posOf (CTokSizeof   pos  ) = pos
  posOf (CTokStatic   pos  ) = pos
  posOf (CTokStruct   pos  ) = pos
  posOf (CTokSwitch   pos  ) = pos
  posOf (CTokTypedef  pos  ) = pos
  posOf (CTokUnion    pos  ) = pos
  posOf (CTokUnsigned pos  ) = pos
  posOf (CTokVoid     pos  ) = pos
  posOf (CTokVolatile pos  ) = pos
  posOf (CTokWhile    pos  ) = pos
  posOf (CTokCLit     pos _) = pos
  posOf (CTokILit     pos _) = pos
  posOf (CTokFLit     pos _) = pos
  posOf (CTokSLit     pos _) = pos
  posOf (CTokIdent    pos _) = pos
  posOf (CTokTyIdent  pos _) = pos
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
  showsPrec _ (CTokAlignof  _  ) = showString "alignof"
  showsPrec _ (CTokAsm      _  ) = showString "asm"
  showsPrec _ (CTokAuto     _  ) = showString "auto"
  showsPrec _ (CTokBreak    _  ) = showString "break"
  showsPrec _ (CTokCase     _  ) = showString "case"
  showsPrec _ (CTokChar     _  ) = showString "char"
  showsPrec _ (CTokConst    _  ) = showString "const"
  showsPrec _ (CTokContinue _  ) = showString "continue"
  showsPrec _ (CTokDefault  _  ) = showString "default"
  showsPrec _ (CTokDouble   _  ) = showString "double"
  showsPrec _ (CTokDo       _  ) = showString "do"
  showsPrec _ (CTokElse     _  ) = showString "else"
  showsPrec _ (CTokEnum     _  ) = showString "enum"
  showsPrec _ (CTokExtern   _  ) = showString "extern"
  showsPrec _ (CTokFloat    _  ) = showString "float"
  showsPrec _ (CTokFor      _  ) = showString "for"
  showsPrec _ (CTokGoto     _  ) = showString "goto"
  showsPrec _ (CTokIf       _  ) = showString "if"
  showsPrec _ (CTokInline   _  ) = showString "inline"
  showsPrec _ (CTokInt      _  ) = showString "int"
  showsPrec _ (CTokLong     _  ) = showString "long"
  showsPrec _ (CTokRegister _  ) = showString "register"
  showsPrec _ (CTokRestrict _  ) = showString "restrict"
  showsPrec _ (CTokReturn   _  ) = showString "return"
  showsPrec _ (CTokShort    _  ) = showString "short"
  showsPrec _ (CTokSigned   _  ) = showString "signed"
  showsPrec _ (CTokSizeof   _  ) = showString "sizeof"
  showsPrec _ (CTokStatic   _  ) = showString "static"
  showsPrec _ (CTokStruct   _  ) = showString "struct"
  showsPrec _ (CTokSwitch   _  ) = showString "switch"
  showsPrec _ (CTokTypedef  _  ) = showString "typedef"
  showsPrec _ (CTokUnion    _  ) = showString "union"
  showsPrec _ (CTokUnsigned _  ) = showString "unsigned"
  showsPrec _ (CTokVoid     _  ) = showString "void"
  showsPrec _ (CTokVolatile _  ) = showString "volatile"
  showsPrec _ (CTokWhile    _  ) = showString "while"
  showsPrec _ (CTokCLit     _ c) = showChar c
  showsPrec _ (CTokILit     _ i) = (showString . show) i
  showsPrec _ (CTokFLit     _ s) = showString s
  showsPrec _ (CTokSLit     _ s) = showString s
  showsPrec _ (CTokIdent    _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokTyIdent  _ i) = (showString . identToLexeme) i
  showsPrec _ (CTokGnuC GnuCAttrTok _) = showString "__attribute__"
  showsPrec _ (CTokGnuC GnuCExtTok  _) = showString "__extension__"

-- We use the odd looking list of string patterns here rather than normal
-- string literals since GHC converts the latter into a sequence of string
-- comparisons (ie a linear search) but it translates the former using its
-- effecient pattern matching which gives us the expected radix-style search.
-- This gives change makes a significant performance difference.
--
idkwtok :: String -> Position -> P CToken
idkwtok ('a':'l':'i':'g':'n':'o':'f':[])		     = tok CTokAlignof
idkwtok ('_':'_':'a':'l':'i':'g':'n':'o':'f':[])	     = tok CTokAlignof
idkwtok ('_':'_':'a':'l':'i':'g':'n':'o':'f':'_':'_':[])     = tok CTokAlignof
idkwtok ('a':'s':'m':[])				     = tok CTokAsm
idkwtok ('_':'_':'a':'s':'m':[])			     = tok CTokAsm
idkwtok ('_':'_':'a':'s':'m':'_':'_':[])		     = tok CTokAsm
idkwtok ('a':'u':'t':'o':[])				     = tok CTokAuto
idkwtok ('b':'r':'e':'a':'k':[])			     = tok CTokBreak
idkwtok ('c':'a':'s':'e':[])				     = tok CTokCase
idkwtok ('c':'h':'a':'r':[])				     = tok CTokChar
idkwtok ('c':'o':'n':'s':'t':[])			     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':[])		     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':'_':'_':[])	     = tok CTokConst
idkwtok ('c':'o':'n':'t':'i':'n':'u':'e':[])		     = tok CTokContinue
idkwtok ('d':'e':'f':'a':'u':'l':'t':[])		     = tok CTokDefault
idkwtok ('d':'o':[])					     = tok CTokDo
idkwtok ('d':'o':'u':'b':'l':'e':[])			     = tok CTokDouble
idkwtok ('e':'l':'s':'e':[])				     = tok CTokElse
idkwtok ('e':'n':'u':'m':[])				     = tok CTokEnum
idkwtok ('e':'x':'t':'e':'r':'n':[])			     = tok CTokExtern
idkwtok ('f':'l':'o':'a':'t':[])			     = tok CTokFloat
idkwtok ('f':'o':'r':[])				     = tok CTokFor
idkwtok ('g':'o':'t':'o':[])				     = tok CTokGoto
idkwtok ('i':'f':[])					     = tok CTokIf
idkwtok ('i':'n':'l':'i':'n':'e':[])			     = tok CTokInline
idkwtok ('_':'_':'i':'n':'l':'i':'n':'e':[])		     = tok CTokInline
idkwtok ('_':'_':'i':'n':'l':'i':'n':'e':'_':'_':[])	     = tok CTokInline
idkwtok ('i':'n':'t':[])				     = tok CTokInt
idkwtok ('l':'o':'n':'g':[])				     = tok CTokLong
idkwtok ('r':'e':'g':'i':'s':'t':'e':'r':[])		     = tok CTokRegister
idkwtok ('r':'e':'s':'t':'r':'i':'c':'t':[])		     = tok CTokRestrict
idkwtok ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':[])	     = tok CTokRestrict
idkwtok ('_':'_':'r':'e':'s':'t':'r':'i':'c':'t':'_':'_':[]) = tok CTokRestrict
idkwtok ('r':'e':'t':'u':'r':'n':[])			     = tok CTokReturn
idkwtok ('s':'h':'o':'r':'t':[])			     = tok CTokShort
idkwtok ('s':'i':'g':'n':'e':'d':[])			     = tok CTokSigned
idkwtok ('_':'_':'s':'i':'g':'n':'e':'d':[])		     = tok CTokSigned
idkwtok ('_':'_':'s':'i':'g':'n':'e':'d':'_':'_':[])	     = tok CTokSigned
idkwtok ('s':'i':'z':'e':'o':'f':[])			     = tok CTokSizeof
idkwtok ('s':'t':'a':'t':'i':'c':[])			     = tok CTokStatic
idkwtok ('s':'t':'r':'u':'c':'t':[])			     = tok CTokStruct
idkwtok ('s':'w':'i':'t':'c':'h':[])			     = tok CTokSwitch
idkwtok ('t':'y':'p':'e':'d':'e':'f':[])		     = tok CTokTypedef
idkwtok ('u':'n':'i':'o':'n':[])			     = tok CTokUnion
idkwtok ('u':'n':'s':'i':'g':'n':'e':'d':[])		     = tok CTokUnsigned
idkwtok ('v':'o':'i':'d':[])				     = tok CTokVoid
idkwtok ('v':'o':'l':'a':'t':'i':'l':'e':[])		     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':[])	     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':'_':'_':[]) = tok CTokVolatile
idkwtok ('w':'h':'i':'l':'e':[])			     = tok CTokWhile
idkwtok ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':'_':'_':[]) =
						tok (CTokGnuC GnuCAttrTok)
idkwtok ('_':'_':'e':'x':'t':'e':'n':'s':'i':'o':'n':'_':'_':[]) =
						tok (CTokGnuC GnuCExtTok)
idkwtok cs = \pos -> do
  name <- getNewName
  tdefs <- getTypedefs
  let ident = lexemeToIdent pos cs name
  if ident `Set.elementOf` tdefs
    then return (CTokTyIdent pos ident)
    else return (CTokIdent   pos ident)

tok :: (Position -> CToken) -> Position -> P CToken
tok tc pos = return (tc pos)

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
			 'x'  -> case head (readHex cs) of
			           (i, cs') -> (toEnum i, cs')
			 _    -> case head (readOct (c:cs)) of
			           (i, cs') -> (toEnum i, cs')
oneChar (c   :cs)    = (c, cs)

normalizeEscapes [] = []
normalizeEscapes cs = case oneChar cs of
                        (c, cs') -> c : normalizeEscapes cs'

adjustPos :: String -> Position -> Position
adjustPos str (fname, row, _) = (fname', row', 0)
  where
    str'            = dropWhite . drop 1 $ str
    (rowStr, str'') = span isDigit str'
    row'	    = read rowStr
    str'''	    = dropWhite str''
    fnameStr	    = takeWhile (/= '"') . drop 1 $ str'''
    fname'	    | null str''' || head str''' /= '"'	= fname
		    -- try and get more sharing of file name strings
		    | fnameStr == fname			= fname
		    | otherwise				= fnameStr
    --
    dropWhite = dropWhile (\c -> c == ' ' || c == '\t')

{-# INLINE token_ #-}
-- token that ignores the string
token_ :: (Position -> CToken) -> Position -> Int -> String -> P CToken
token_ tok pos _ _ = return (tok pos)

{-# INLINE token #-}
-- token that uses the string
token :: (Position -> a -> CToken) -> (String -> a)
      -> Position -> Int -> String -> P CToken
token tok read pos len str = return (tok pos (read $ take len str))


-- -----------------------------------------------------------------------------
-- The input type

type AlexInput = (Position, 	-- current position,
		  String)	-- current input string

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar _ = error "alexInputPrevChar not used"

alexGetChar :: AlexInput -> Maybe (Char,AlexInput)
alexGetChar (p,[]) = Nothing
alexGetChar (p,(c:s))  = let p' = alexMove p c in p' `seq`
                           Just (c, (p', s))

alexMove :: Position -> Char -> Position
alexMove (f, l, c) '\t' = (f,  l,    (((c+7) `div` 8)*8+1))
alexMove (f, l, c) '\n' = (f, (l+1),  1)
alexMove (f, l, c) _    = (f,  l,    (c+1))


-- -----------------------------------------------------------------------------
-- The lexer & parser monad

data ParseResult a
  = POk !PState a
  | PFailed [String] Position	-- The error message and position

data PState = PState { 
	alex_pos :: !Position,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_last  :: CToken,	-- the previous token
	alex_names :: [Name],	-- the name unique supply
	alex_tdefs :: Set Ident	-- the set of typedef'ed identifiers
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail m = getPos >>= \pos -> failP pos [m]

execParser :: P a -> String -> Position -> [Ident] -> [Name]
           -> Either a ([String], Position)
execParser (P parser) input pos builtins names =
  case parser initialState of
    POk _ result -> Left result
    PFailed message pos -> Right (message, pos)
  where initialState = PState {
          alex_pos = pos,
	  alex_inp = input,
	  alex_last = interr "CLexer.execParser: Touched undefined token!",
	  alex_names = names,
	  alex_tdefs = Set.mkSet builtins
        }

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
	case m s of
		POk s' a        -> (unP (k a)) s'
		PFailed err pos -> PFailed err pos

failP :: Position -> [String] -> P a
failP pos msg = P $ \_ -> PFailed msg pos

lexicalError :: P a
lexicalError = do
  (pos, (c:cs)) <- getInput
  failP pos
        ["Lexical error!",
         "The character " ++ show c ++ " does not fit here."]

parseError :: P a
parseError = do
  tok <- getLastToken
  failP (posOf tok)
        ["Syntax error!",
         "The symbol `" ++ show tok ++ "' does not fit here."]

getNewName :: P Name
getNewName = P $ \s@PState{alex_names=(n:ns)} -> POk s{alex_names=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{alex_pos=pos} ()

getPos :: P Position
getPos = P $ \s@PState{alex_pos=pos} -> POk s pos

getTypedefs :: P (Set Ident)
getTypedefs = P $ \s@PState{alex_tdefs=tdefs} -> POk s tdefs

addTypedef :: Ident -> P ()
addTypedef ident = (P $ \s@PState{alex_tdefs=tdefs} ->
                             POk s{alex_tdefs = tdefs `Set.addToSet` ident} ())

getInput :: P AlexInput
getInput = P $ \s@PState{alex_pos=p, alex_inp=i} -> POk s (p,i)

setInput :: AlexInput -> P ()
setInput (p,i) = P $ \s -> POk s{alex_pos=p, alex_inp=i} ()

getLastToken :: P CToken
getLastToken = P $ \s@PState{alex_last=tok} -> POk s tok

setLastToken :: CToken -> P ()
setLastToken tok = P $ \s -> POk s{alex_last=tok} ()

lexToken :: P CToken
lexToken = do
  inp@(pos, str) <- getInput
  case alexScan inp 0 of
    AlexEOF -> return CTokEof
    AlexError inp' -> lexicalError
    AlexSkip  inp' len -> do
	setInput inp'
	lexToken
    AlexToken inp' len action -> do
	setInput inp'
	tok <- action pos len str
	setLastToken tok
	return tok

lexC :: (CToken -> P a) -> P a
lexC cont = do
  tok <- lexToken
  cont tok
}
