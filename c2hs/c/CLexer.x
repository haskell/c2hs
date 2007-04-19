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

module CLexer (lexC, parseError) where

import Char      (isDigit)
import Numeric   (readDec, readOct, readHex)

import Position  (Position(..), Pos(posOf))
import Errors    (interr)
import UNames	 (Name)
import Idents    (Ident, lexemeToIdent, identToLexeme)

import CTokens
import CParserMonad

}

$space = [ \ \t ] -- horazontal white space
$eol   = \n

$letter   = [a-zA-Z_]
$octdigit = 0-7
$digit    = 0-9
$digitNZ  = 1-9
$hexdigit = [0-9a-fA-F]

$inchar   = \0-\255 # [ \\ \' \n \r ]
$instr    = \0-\255 # [ \\ \" \n \r ]
$anyButNL = \0-\255 # \n
$infname  = \ -\127 # [ \\ \" ]
$visible  = \ -\127

@int = $digitNZ$digit*
@sp  = $space*

-- character escape sequence (follows K&R A2.5.2)
--
-- * also used for strings
--
@charesc  = \\([ntvbrfae\\\?\'\"]|$octdigit{1,3}|x$hexdigit+)

-- components of float constants (follows K&R A2.5.3)
--
@digits    = $digit+
@intpart   = @digits
@fractpart = @digits
@mantpart  = @intpart?\.@fractpart|@intpart\.
@exppart   = [eE][\+\-]?@digits
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
\#$space*@int$space*(\"($infname|@charesc)*\"$space*)?(@int$space*)*$eol
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
0$octdigit*[uUlL]{0,3}		{ token CTokILit (fst . head . readOct) }
$digitNZ$digit*[uUlL]{0,3}	{ token CTokILit (fst . head . readDec) }
0[xX]$hexdigit*[uUlL]{0,3}	{ token CTokILit (fst . head . readHex . drop 2) }

-- character constants (follows K&R A2.5.2)
--
\'($inchar|@charesc)\'	{ token CTokCLit (fst . oneChar . tail) }
L\'($inchar|@charesc)\'	{ token CTokCLit (fst . oneChar . tail . tail) }

-- float constants (follows K&R A2.5.3)
--
(@mantpart@exppart?|@intpart@exppart)@suffix?	{ token CTokFLit id }

-- string literal (follows K&R A2.6)
--
\"($instr|@charesc)*\"			{ token CTokSLit normalizeEscapes }
L\"($instr|@charesc)*\"			{ token CTokSLit (normalizeEscapes . tail) }


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
idkwtok ('_':'B':'o':'o':'l':[])			     = tok CTokBool
idkwtok ('c':'a':'s':'e':[])				     = tok CTokCase
idkwtok ('c':'h':'a':'r':[])				     = tok CTokChar
idkwtok ('c':'o':'n':'s':'t':[])			     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':[])		     = tok CTokConst
idkwtok ('_':'_':'c':'o':'n':'s':'t':'_':'_':[])	     = tok CTokConst
idkwtok ('c':'o':'n':'t':'i':'n':'u':'e':[])		     = tok CTokContinue
idkwtok ('_':'C':'o':'m':'p':'l':'e':'x':[])		     = tok CTokComplex
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
idkwtok ('t':'y':'p':'e':'o':'f':[])			     = tok CTokTypeof
idkwtok ('_':'_':'t':'y':'p':'e':'o':'f':[])		     = tok CTokTypeof
idkwtok ('_':'_':'t':'y':'p':'e':'o':'f':'_':'_':[])	     = tok CTokTypeof
idkwtok ('_':'_':'t':'h':'r':'e':'a':'d':[])		     = tok CTokThread
idkwtok ('u':'n':'i':'o':'n':[])			     = tok CTokUnion
idkwtok ('u':'n':'s':'i':'g':'n':'e':'d':[])		     = tok CTokUnsigned
idkwtok ('v':'o':'i':'d':[])				     = tok CTokVoid
idkwtok ('v':'o':'l':'a':'t':'i':'l':'e':[])		     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':[])	     = tok CTokVolatile
idkwtok ('_':'_':'v':'o':'l':'a':'t':'i':'l':'e':'_':'_':[]) = tok CTokVolatile
idkwtok ('w':'h':'i':'l':'e':[])			     = tok CTokWhile
idkwtok ('_':'_':'l':'a':'b':'e':'l':'_':'_':[])             = tok CTokLabel
idkwtok ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':[]) = tok (CTokGnuC GnuCAttrTok)
--						ignoreAttribute >> lexToken
idkwtok ('_':'_':'a':'t':'t':'r':'i':'b':'u':'t':'e':'_':'_':[]) = tok (CTokGnuC GnuCAttrTok)
--						ignoreAttribute >> lexToken
idkwtok ('_':'_':'e':'x':'t':'e':'n':'s':'i':'o':'n':'_':'_':[]) =
						tok (CTokGnuC GnuCExtTok)
idkwtok ('_':'_':'b':'u':'i':'l':'t':'i':'n':'_':rest)
        | rest == "va_arg"             = tok (CTokGnuC GnuCVaArg)
        | rest == "offsetof"           = tok (CTokGnuC GnuCOffsetof)
        | rest == "types_compatible_p" = tok (CTokGnuC GnuCTyCompat)

idkwtok cs = \pos -> do
  name <- getNewName
  let ident = lexemeToIdent pos cs name
  tyident <- isTypeIdent ident
  if tyident
    then return (CTokTyIdent pos ident)
    else return (CTokIdent   pos ident)

ignoreAttribute :: P ()
ignoreAttribute = skipTokens 0
  where skipTokens n = do
          tok <- lexToken
          case tok of
            CTokRParen _ | n == 1    -> return ()
                         | otherwise -> skipTokens (n-1)
            CTokLParen _             -> skipTokens (n+1)
            _                        -> skipTokens n

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
			 'e'  -> ('\ESC', cs)  --GNU C extension
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
adjustPos str (Position fname row _) = Position fname' row' 0
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
alexMove (Position f l c) '\t' = Position f l     (((c+7) `div` 8)*8+1)
alexMove (Position f l c) '\n' = Position f (l+1) 1
alexMove (Position f l c) _    = Position f l     (c+1)

lexicalError :: P a
lexicalError = do
  pos <- getPos
  (c:cs) <- getInput
  failP pos
        ["Lexical error!",
         "The character " ++ show c ++ " does not fit here."]

parseError :: P a
parseError = do
  tok <- getLastToken
  failP (posOf tok)
        ["Syntax error!",
         "The symbol `" ++ show tok ++ "' does not fit here."]

lexToken :: P CToken
lexToken = do
  pos <- getPos
  inp <- getInput
  case alexScan (pos, inp) 0 of
    AlexEOF -> return CTokEof
    AlexError inp' -> lexicalError
    AlexSkip  (pos', inp') len -> do
        setPos pos'
        setInput inp'
	lexToken
    AlexToken (pos', inp') len action -> do
        setPos pos'
        setInput inp'
        tok <- action pos len inp
        setLastToken tok
        return tok

lexC :: (CToken -> P a) -> P a
lexC cont = do
  tok <- lexToken
  cont tok
}
