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
--  C Tokens for the C lexer.
--

module CTokens (CToken(..), GnuCTok(..)) where 

import Position  (Position(..), Pos(posOf))
import Idents    (Ident, identToLexeme)


-- token definition
-- ----------------

-- possible tokens (EXPORTED)
--
data CToken = CTokLParen   !Position		-- `('
	    | CTokRParen   !Position		-- `)'
	    | CTokLBracket !Position		-- `['
	    | CTokRBracket !Position		-- `]'
	    | CTokArrow	   !Position		-- `->'
	    | CTokDot	   !Position		-- `.'
	    | CTokExclam   !Position		-- `!'
	    | CTokTilde	   !Position		-- `~'
	    | CTokInc	   !Position		-- `++'
	    | CTokDec	   !Position		-- `--'
	    | CTokPlus	   !Position		-- `+'
	    | CTokMinus	   !Position		-- `-'
	    | CTokStar	   !Position		-- `*'
	    | CTokSlash	   !Position		-- `/'
	    | CTokPercent  !Position		-- `%'
	    | CTokAmper    !Position		-- `&'
	    | CTokShiftL   !Position		-- `<<'
	    | CTokShiftR   !Position		-- `>>'
	    | CTokLess	   !Position		-- `<'
	    | CTokLessEq   !Position		-- `<='
	    | CTokHigh	   !Position		-- `>'
	    | CTokHighEq   !Position		-- `>='
	    | CTokEqual	   !Position		-- `=='
	    | CTokUnequal  !Position		-- `!='
	    | CTokHat	   !Position		-- `^'
	    | CTokBar	   !Position		-- `|'
	    | CTokAnd	   !Position		-- `&&'
	    | CTokOr	   !Position		-- `||'
	    | CTokQuest	   !Position		-- `?'
	    | CTokColon	   !Position		-- `:'
	    | CTokAssign   !Position		-- `='
	    | CTokPlusAss  !Position		-- `+='
	    | CTokMinusAss !Position		-- `-='
	    | CTokStarAss  !Position		-- `*='
	    | CTokSlashAss !Position		-- `/='
	    | CTokPercAss  !Position		-- `%='
	    | CTokAmpAss   !Position		-- `&='
	    | CTokHatAss   !Position		-- `^='
	    | CTokBarAss   !Position		-- `|='
	    | CTokSLAss	   !Position		-- `<<='
	    | CTokSRAss	   !Position		-- `>>='
	    | CTokComma    !Position		-- `,'
	    | CTokSemic    !Position		-- `;'
	    | CTokLBrace   !Position		-- `{'
	    | CTokRBrace   !Position		--
	    | CTokEllipsis !Position		-- `...'
	    | CTokAlignof  !Position		-- `alignof' 
						-- (or `__alignof', 
						-- `__alignof__')
	    | CTokAsm      !Position		-- `asm'
	    					-- (or `__asm',
						-- `__asm__')
	    | CTokAuto     !Position		-- `auto'
	    | CTokBreak    !Position		-- `break'
	    | CTokBool     !Position		-- `_Bool'
	    | CTokCase     !Position		-- `case'
	    | CTokChar     !Position		-- `char'
	    | CTokConst    !Position		-- `const' 
						-- (or `__const', `__const__')
	    | CTokContinue !Position		-- `continue' 
	    | CTokComplex  !Position		-- `_Complex' 
	    | CTokDefault  !Position		-- `default'
	    | CTokDo       !Position		-- `do'
	    | CTokDouble   !Position		-- `double'
	    | CTokElse     !Position		-- `else'
	    | CTokEnum     !Position		-- `enum'
	    | CTokExtern   !Position		-- `extern'
 	    | CTokFloat    !Position		-- `float'
 	    | CTokFor      !Position		-- `for'
 	    | CTokGoto     !Position		-- `goto'
 	    | CTokIf       !Position		-- `if'
	    | CTokInline   !Position		-- `inline'
						-- (or `__inline', 
						-- `__inline__')
	    | CTokInt      !Position		-- `int'
	    | CTokLong     !Position		-- `long'
	    | CTokLabel    !Position		-- `__label__'
	    | CTokRegister !Position		-- `register'
	    | CTokRestrict !Position		-- `restrict'
						-- (or `__restrict', 
						-- `__restrict__')
	    | CTokReturn   !Position		-- `return'
	    | CTokShort    !Position		-- `short'
	    | CTokSigned   !Position		-- `signed'
						-- (or `__signed', 
						-- `__signed__')
	    | CTokSizeof   !Position		-- `sizeof'
	    | CTokStatic   !Position		-- `static'
	    | CTokStruct   !Position		-- `struct'
	    | CTokSwitch   !Position		-- `switch'
	    | CTokTypedef  !Position		-- `typedef'
	    | CTokTypeof   !Position		-- `typeof'
	    | CTokThread   !Position		-- `__thread'
	    | CTokUnion    !Position		-- `union'
	    | CTokUnsigned !Position		-- `unsigned'
	    | CTokVoid     !Position		-- `void'
	    | CTokVolatile !Position		-- `volatile'
						-- (or `__volatile', 
						-- `__volatile__')
	    | CTokWhile    !Position		-- `while'
	    | CTokCLit	   !Position !Char	-- character constant
	    | CTokILit	   !Position !Integer	-- integer constant
	    | CTokFLit	   !Position String	-- float constant
	    | CTokSLit	   !Position String	-- string constant (no escapes)
	    | CTokIdent	   !Position !Ident	-- identifier

	      -- not generated here, but in `CParser.parseCHeader'
	    | CTokTyIdent  !Position !Ident	-- `typedef-name' identifier
	    | CTokGnuC !GnuCTok !Position	-- special GNU C tokens
	    | CTokEof				-- end of file

-- special tokens used in GNU C extensions to ANSI C
--
data GnuCTok = GnuCAttrTok		-- `__attribute__'
	     | GnuCExtTok		-- `__extension__'
	     | GnuCVaArg		-- `__builtin_va_arg'
       	     | GnuCOffsetof		-- `__builtin_offsetof'
	     | GnuCTyCompat		-- `__builtin_types_compatible_p'

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
  posOf (CTokBool     pos  ) = pos
  posOf (CTokCase     pos  ) = pos
  posOf (CTokChar     pos  ) = pos
  posOf (CTokConst    pos  ) = pos
  posOf (CTokContinue pos  ) = pos
  posOf (CTokComplex  pos  ) = pos
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
  posOf (CTokLabel    pos  ) = pos
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
  posOf (CTokTypeof   pos  ) = pos
  posOf (CTokThread   pos  ) = pos
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
  showsPrec _ (CTokLabel    _  ) = showString "__label__"
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
  showsPrec _ (CTokTypeof   _  ) = showString "typeof"
  showsPrec _ (CTokThread   _  ) = showString "__thread"
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
  showsPrec _ (CTokGnuC GnuCVaArg    _) = showString "__builtin_va_arg"
  showsPrec _ (CTokGnuC GnuCOffsetof _) = showString "__builtin_offsetof"
  showsPrec _ (CTokGnuC GnuCTyCompat _) = showString "__builtin_types_compatible_p"
