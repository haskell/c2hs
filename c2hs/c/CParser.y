--  C -> Haskell Compiler: Parser for C Header Files
--
--  Author : Duncan Coutts, Manuel M T Chakravarty
--  Created: 29 May 2005
--
--  Copyright (c) 2005-2007 Duncan Coutts
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--  Portions Copyright (c) 1989, 1990 James A. Roskind
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
--  Parser for C translation units, which have already been run through the C
--  preprocessor.  
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The parser recognizes all of ISO C 99 and most common GNU C extensions.
--
--  With C99 we refer to the ISO C99 standard, specifically the section numbers
--  used below refer to this report:
--
--    http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf
--
--
--  Since some of the grammar productions are quite difficult to read
--  (especially those involved with the decleration syntax) we document them
--  with an extended syntax that allows a more consise representation:
--
--  Ordinary rules
--
--   foo      named terminal or non-terminal
--
--   'c'      terminal, literal character token
--
--   A B      concatenation
--
--   A | B    alternation
--
--   (A)      grouping
--
--  Extended rules
--
--   A?       optional, short hand for (A|) or [A]{ 0==A || 1==A }
--
--   ...      stands for some part of the grammar omitted for clarity
--
--   [A]      represents sequences, 0 or more.
--
--   [A]{C}   sequences with some constraint, usually on the number of
--            terminals or non-terminals appearing in the sequence.
--
--  Constraints on sequences
--
--   n==t     terminal or non-terminal t must appear exactly n times
--
--   n>=t     terminal or non-terminal t must appear at least n times
--
--   C1 && C1 conjunction of constraints
--
--   C1 || C2 disjunction of constraints
--
--   C1 |x| C2 exclusive disjunction of constraints
--
--
--  Comments:
--
--  * Subtrees representing empty declarators of the form `CVarDeclr Nothing
--    at' have *no* valid attribute handle in `at' (only a `newAttrsOnlyPos
--    nopos').
--
--  * Builtin type names are imported from `CBuiltin'.
--
--- TODO ----------------------------------------------------------------------
--
--  * GNUC __attribute__s should be enetered into the parse tree since they
--    contain useful api/abi information.
--
--  * Some other extensions are currently recognised by the parser but not
--    entered into the parse tree.
--

{
module CParser (parseC) where

import Prelude    hiding (reverse)
import qualified Data.List as List

import Position   (Position, Pos(..), nopos)
import UNames     (names)
import Idents     (Ident)
import Attributes (Attrs, newAttrs, newAttrsOnlyPos)

import State      (PreCST, raiseFatal, getNameSupply)
import CLexer     (lexC, parseError)
import CAST       (CHeader(..), CExtDecl(..), CFunDef(..), CStat(..),
                   CBlockItem(..), CDecl(..), CDeclSpec(..), CStorageSpec(..),
                   CTypeSpec(..), CTypeQual(..), CStructUnion(..),
                   CStructTag(..), CEnum(..), CDeclr(..), CInit(..), CInitList,
                   CDesignator(..), CExpr(..), CAssignOp(..), CBinaryOp(..),
                   CUnaryOp(..), CConst (..))
import CBuiltin   (builtinTypeNames)
import CTokens    (CToken(..), GnuCTok(..))
import CParserMonad (P, execParser, getNewName, addTypedef, shadowTypedef,
                     enterScope, leaveScope )
}

%name header header
%tokentype { CToken }

%monad { P } { >>= } { return }
%lexer { lexC } { CTokEof }

%expect 1

%token

'('		{ CTokLParen	_ }
')'		{ CTokRParen	_ }
'['		{ CTokLBracket	_ }
']'		{ CTokRBracket	_ }
"->"		{ CTokArrow	_ }
'.'		{ CTokDot	_ }
'!'		{ CTokExclam	_ }
'~'		{ CTokTilde	_ }
"++"		{ CTokInc	_ }
"--"		{ CTokDec	_ }
'+'		{ CTokPlus	_ }
'-'		{ CTokMinus	_ }
'*'		{ CTokStar	_ }
'/'		{ CTokSlash	_ }
'%'		{ CTokPercent	_ }
'&'		{ CTokAmper	_ }
"<<"		{ CTokShiftL	_ }
">>"		{ CTokShiftR	_ }
'<'		{ CTokLess	_ }
"<="		{ CTokLessEq	_ }
'>'		{ CTokHigh	_ }
">="		{ CTokHighEq	_ }
"=="		{ CTokEqual	_ }
"!="		{ CTokUnequal	_ }
'^'		{ CTokHat	_ }
'|'		{ CTokBar	_ }
"&&"		{ CTokAnd	_ }
"||"		{ CTokOr	_ }
'?'		{ CTokQuest	_ }
':'		{ CTokColon	_ }
'='		{ CTokAssign	_ }
"+="		{ CTokPlusAss	_ }
"-="		{ CTokMinusAss	_ }
"*="		{ CTokStarAss	_ }
"/="		{ CTokSlashAss	_ }
"%="		{ CTokPercAss	_ }
"&="		{ CTokAmpAss	_ }
"^="		{ CTokHatAss	_ }
"|="		{ CTokBarAss	_ }
"<<="		{ CTokSLAss	_ }
">>="		{ CTokSRAss	_ }
','		{ CTokComma	_ }
';'		{ CTokSemic	_ }
'{'		{ CTokLBrace	_ }
'}'		{ CTokRBrace	_ }
"..."		{ CTokEllipsis	_ }
alignof		{ CTokAlignof	_ }
asm		{ CTokAsm	_ }
auto		{ CTokAuto	_ }
break		{ CTokBreak	_ }
"_Bool"		{ CTokBool	_ }
case		{ CTokCase	_ }
char		{ CTokChar	_ }
const		{ CTokConst	_ }
continue	{ CTokContinue	_ }
"_Complex"	{ CTokComplex	_ }
default		{ CTokDefault	_ }
do		{ CTokDo	_ }
double		{ CTokDouble	_ }
else		{ CTokElse	_ }
enum		{ CTokEnum	_ }
extern		{ CTokExtern	_ }
float		{ CTokFloat	_ }
for		{ CTokFor	_ }
goto		{ CTokGoto	_ }
if		{ CTokIf	_ }
inline		{ CTokInline	_ }
int		{ CTokInt	_ }
long		{ CTokLong	_ }
"__label__"	{ CTokLabel	_ }
register	{ CTokRegister	_ }
restrict	{ CTokRestrict	_ }
return		{ CTokReturn	_ }
short		{ CTokShort	_ }
signed		{ CTokSigned	_ }
sizeof		{ CTokSizeof	_ }
static		{ CTokStatic	_ }
struct		{ CTokStruct	_ }
switch		{ CTokSwitch	_ }
typedef		{ CTokTypedef	_ }
typeof		{ CTokTypeof	_ }
"__thread"	{ CTokThread	_ }
union		{ CTokUnion	_ }
unsigned	{ CTokUnsigned	_ }
void		{ CTokVoid	_ }
volatile	{ CTokVolatile	_ }
while		{ CTokWhile	_ }
cchar		{ CTokCLit   _ _ }		-- character constant
cint		{ CTokILit   _ _ }		-- integer constant
cfloat		{ CTokFLit   _ _ }		-- float constant
cstr		{ CTokSLit   _ _ }		-- string constant (no escapes)
ident		{ CTokIdent  _ $$ }		-- identifier
tyident		{ CTokTyIdent _ $$ }		-- `typedef-name' identifier
"__attribute__"	{ CTokGnuC GnuCAttrTok _ }	-- special GNU C tokens
"__extension__"	{ CTokGnuC GnuCExtTok  _ }	-- special GNU C tokens

-- special GNU C builtin 'functions' that actually take types as parameters:
"__builtin_va_arg"		{ CTokGnuC GnuCVaArg    _ }
"__builtin_offsetof"		{ CTokGnuC GnuCOffsetof _ }
"__builtin_types_compatible_p"	{ CTokGnuC GnuCTyCompat _ }

%%


-- parse a complete C header file
--
header :: { CHeader }
header
  : translation_unit	{% withAttrs $1 $ CHeader (reverse $1) }


-- parse a complete C translation unit (C99 6.9)
--
-- * GNU extensions:
--     allow empty translation_unit
--     allow redundant ';'
--
translation_unit :: { Reversed [CExtDecl] }
translation_unit
  : {- empty -}					{ empty }
  | translation_unit ';'			{ $1 }
  | translation_unit external_declaration	{ $1 `snoc` $2 }


-- parse external C declaration (C99 6.9)
--
-- * GNU extensions:
--     allow extension keyword before external declaration
--     asm definitions
--
external_declaration :: { CExtDecl }
external_declaration
  : attrs_opt function_definition		{ CFDefExt $2 }
  | attrs_opt declaration			{ CDeclExt $2 }
  | "__extension__" external_declaration	{ $2 }
  | asm '(' string_literal ')' ';'		{% withAttrs $2 CAsmExt }


-- parse C function definition (C99 6.9.1)
--
function_definition :: { CFunDef }
function_definition
  :                            function_declarator compound_statement
  	{% leaveScope >> (withAttrs $1 $ CFunDef [] $1 [] $2) }

  | declaration_specifier      function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef $1 $2 [] $3) }

  | type_specifier             function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef $1 $2 [] $3) }

  | declaration_qualifier_list function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef (reverse $1) $2 [] $3) }

  | type_qualifier_list        function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef (liftTypeQuals $1) $2 [] $3) }

  |                            old_function_declarator declaration_list compound_statement
  	{% withAttrs $1 $ CFunDef [] $1 (reverse $2) $3 }

  | declaration_specifier      old_function_declarator declaration_list compound_statement
  	{% withAttrs $1 $ CFunDef $1 $2 (reverse $3) $4 }

  | type_specifier             old_function_declarator declaration_list compound_statement
  	{% withAttrs $1 $ CFunDef $1 $2 (reverse $3) $4 }

  | declaration_qualifier_list old_function_declarator declaration_list compound_statement
  	{% withAttrs $1 $ CFunDef (reverse $1) $2 (reverse $3) $4 }

  | type_qualifier_list        old_function_declarator declaration_list compound_statement
  	{% withAttrs $1 $ CFunDef (liftTypeQuals $1) $2 (reverse $3) $4 }


function_declarator :: { CDeclr }
function_declarator
  : identifier_declarator
  	{% enterScope >> doFuncParamDeclIdent $1 >> return $1 }


declaration_list :: { Reversed [CDecl] }
declaration_list
  : {- empty -}					{ empty }
  | declaration_list declaration		{ $1 `snoc` $2 }


-- parse C statement (C99 6.8)
--
-- * GNU extension: ' __asm__ (...); ' statements
--
statement :: { CStat }
statement
  : labeled_statement			{ $1 }
  | compound_statement			{ $1 }
  | expression_statement		{ $1 }
  | selection_statement			{ $1 }
  | iteration_statement			{ $1 }
  | jump_statement			{ $1 }
  | asm_statement			{ $1 }


-- parse C labeled statement (C99 6.8.1)
--
-- * GNU extension: case ranges
--
labeled_statement :: { CStat }
labeled_statement
  : identifier ':' attrs_opt statement		{% withAttrs $2 $ CLabel $1 $4}
  | case constant_expression ':' statement	{% withAttrs $1 $ CCase $2 $4 }
  | default ':' statement			{% withAttrs $1 $ CDefault $3 }
  | case constant_expression "..." constant_expression ':' statement
  	{% withAttrs $1 $ CCases $2 $4 $6 }


-- parse C compound statement (C99 6.8.2)
--
-- * GNU extension: '__label__ ident;' declarations
--
compound_statement :: { CStat }
compound_statement
  : '{' enter_scope block_item_list leave_scope '}'
  	{% withAttrs $1 $ CCompound (reverse $3) }

  | '{' enter_scope label_declarations block_item_list leave_scope '}'
  	{% withAttrs $1 $ CCompound (reverse $4) }


-- No syntax for these, just side effecting semantic actions.
--
enter_scope :: { () }
enter_scope : {% enterScope }
leave_scope :: { () }
leave_scope : {% leaveScope }


block_item_list :: { Reversed [CBlockItem] }
block_item_list
  : {- empty -}			{ empty }
  | block_item_list block_item	{ $1 `snoc` $2 }


block_item :: { CBlockItem }
block_item
  : statement			{ CBlockStmt $1 }
  | nested_declaration		{ $1 }


nested_declaration :: { CBlockItem }
nested_declaration
  : declaration				{ CBlockDecl $1 }
  | attrs declaration			{ CBlockDecl $2 }
  | nested_function_definition		{ CNestedFunDef $1 }
  | attrs nested_function_definition	{ CNestedFunDef $2 }
  | "__extension__" nested_declaration	{ $2 }


nested_function_definition :: { CFunDef }
nested_function_definition
  : declaration_specifier      function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef $1 $2 [] $3) }

  | type_specifier             function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef $1 $2 [] $3) }

  | declaration_qualifier_list function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef (reverse $1) $2 [] $3) }

  | type_qualifier_list        function_declarator compound_statement
	{% leaveScope >> (withAttrs $1 $ CFunDef (liftTypeQuals $1) $2 [] $3) }


label_declarations :: { () }
label_declarations
  : "__label__" identifier_list ';'			{ () }
  | label_declarations "__label__" identifier_list ';'	{ () }


-- parse C expression statement (C99 6.8.3)
--
expression_statement :: { CStat }
expression_statement
  : ';'				{% withAttrs $1 $ CExpr Nothing }
  | expression ';'		{% withAttrs $1 $ CExpr (Just $1) }


-- parse C selection statement (C99 6.8.4)
--
selection_statement :: { CStat }
selection_statement
  : if '(' expression ')' statement
	{% withAttrs $1 $ CIf $3 $5 Nothing }

  | if '(' expression ')' statement else statement
	{% withAttrs $1 $ CIf $3 $5 (Just $7) }

  | switch '(' expression ')' statement	
	{% withAttrs $1 $ CSwitch $3 $5 }


-- parse C iteration statement (C99 6.8.5)
--
iteration_statement :: { CStat }
iteration_statement
  : while '(' expression ')' statement
  	{% withAttrs $1 $ CWhile $3 $5 False }

  | do statement while '(' expression ')' ';'
  	{% withAttrs $1 $ CWhile $5 $2 True }

  | for '(' expression_opt ';' expression_opt ';' expression_opt ')' statement
	{% withAttrs $1 $ CFor (Left $3) $5 $7 $9 }

  | for '(' enter_scope declaration expression_opt ';' expression_opt ')' statement leave_scope
	{% withAttrs $1 $ CFor (Right $4) $5 $7 $9 }


-- parse C jump statement (C99 6.8.6)
--
-- * GNU extension: computed gotos
--
jump_statement :: { CStat }
jump_statement
  : goto identifier ';'			{% withAttrs $1 $ CGoto $2 }
  | goto '*' expression ';'		{% withAttrs $1 $ CGotoPtr $3 }
  | continue ';'			{% withAttrs $1 $ CCont }
  | break ';'				{% withAttrs $1 $ CBreak }
  | return expression_opt ';'		{% withAttrs $1 $ CReturn $2 }


-- parse GNU C __asm__ (...) statement (recording only a place holder result)
--
asm_statement :: { CStat }
asm_statement
  : asm maybe_type_qualifier '(' expression ')' ';'
  	{% withAttrs $1 CAsm }

  | asm maybe_type_qualifier '(' expression ':' asm_operands ')' ';'
  	{% withAttrs $1 CAsm }

  | asm maybe_type_qualifier '(' expression ':' asm_operands
					    ':' asm_operands ')' ';'
  	{% withAttrs $1 CAsm }
  | asm maybe_type_qualifier '(' expression ':' asm_operands ':' asm_operands
					    ':' asm_clobbers ')' ';'
  	{% withAttrs $1 CAsm }


maybe_type_qualifier :: { () }
maybe_type_qualifier
  : {- empty -}		{ () }
  | type_qualifier	{ () }


asm_operands :: { () }
asm_operands
  : {- empty -}				{ () }
  | nonnull_asm_operands		{ () }


nonnull_asm_operands :: { () }
nonnull_asm_operands
  : asm_operand					{ () }
  | nonnull_asm_operands ',' asm_operand	{ () }


asm_operand :: { () }
asm_operand
  : string_literal '(' expression ')'			{ () }
  | '[' ident ']' string_literal '(' expression ')'	{ () }
  | '[' tyident ']' string_literal '(' expression ')'	{ () }


asm_clobbers :: { () }
asm_clobbers
  : string_literal			{ () }
  | asm_clobbers ',' string_literal	{ () }


-- parse C declaration (C99 6.7)
--
declaration :: { CDecl }
declaration
  : sue_declaration_specifier ';'
  	{% withAttrs $1 $ CDecl (reverse $1) [] }

  | sue_type_specifier ';'
  	{% withAttrs $1 $ CDecl (reverse $1) [] }

  | declaring_list ';'
  	{ case $1 of
            CDecl declspecs dies attr ->
              CDecl declspecs (List.reverse dies) attr }

  | default_declaring_list ';'
  	{ case $1 of
            CDecl declspecs dies attr ->
              CDecl declspecs (List.reverse dies) attr }


-- Note that if a typedef were redeclared, then a declaration
-- specifier must be supplied
--
-- Can't redeclare typedef names
--
default_declaring_list :: { CDecl }
default_declaring_list
  : declaration_qualifier_list identifier_declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% let declspecs = reverse $1 in
           doDeclIdent declspecs $2
        >> (withAttrs $1 $ CDecl declspecs [(Just $2, $5, Nothing)]) }

  | type_qualifier_list identifier_declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% let declspecs = liftTypeQuals $1 in
           doDeclIdent declspecs $2
        >> (withAttrs $1 $ CDecl declspecs [(Just $2, $5, Nothing)]) }

  | default_declaring_list ',' identifier_declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% case $1 of
             CDecl declspecs dies attr -> do
               doDeclIdent declspecs $3
               return (CDecl declspecs ((Just $3, $6, Nothing) : dies) attr) }


declaring_list :: { CDecl }
declaring_list
  : declaration_specifier declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% doDeclIdent $1 $2
        >> (withAttrs $1 $ CDecl $1 [(Just $2, $5, Nothing)]) }

  | type_specifier declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% doDeclIdent $1 $2
        >> (withAttrs $1 $ CDecl $1 [(Just $2, $5, Nothing)]) }

  | declaring_list ',' declarator asm_opt attrs_opt {-{}-} initializer_opt
  	{% case $1 of
             CDecl declspecs dies attr -> do
               doDeclIdent declspecs $3
               return (CDecl declspecs ((Just $3, $6, Nothing) : dies) attr) }


-- parse C declaration specifiers (C99 6.7)
--
-- * summary:
--   [ type_qualifier | storage_class
--   | basic_type_name | elaborated_type_name | tyident ]{
--     (    1 >= basic_type_name
--      |x| 1 == elaborated_type_name
--      |x| 1 == tyident
--     ) && 1 >= storage_class
--   }
--
declaration_specifier :: { [CDeclSpec] }
declaration_specifier
  : basic_declaration_specifier		{ reverse $1 }	-- Arithmetic or void
  | sue_declaration_specifier		{ reverse $1 }	-- Struct/Union/Enum
  | typedef_declaration_specifier	{ reverse $1 }	-- Typedef


-- A mixture of type qualifiers and storage class specifiers in any order, but
-- containing at least one storage class specifier.
--
-- * summary:
--   [type_qualifier | storage_class]{ 1 >= storage_class }
--
-- * detail:
--   [type_qualifier] storage_class [type_qualifier | storage_class]
--
declaration_qualifier_list :: { Reversed [CDeclSpec] }
declaration_qualifier_list
  : storage_class
  	{ singleton (CStorageSpec $1) }

  | type_qualifier_list storage_class
  	{ rmap CTypeQual $1 `snoc` CStorageSpec $2 }

  | declaration_qualifier_list declaration_qualifier
  	{ $1 `snoc` $2 }

  | declaration_qualifier_list attr
  	{ $1 }


declaration_qualifier :: { CDeclSpec }
declaration_qualifier
  : storage_class		{ CStorageSpec $1 }
  | type_qualifier		{ CTypeQual $1 }     -- const or volatile


-- parse C storage class specifier (C99 6.7.1)
--
-- * GNU extensions: '__thread' thread local storage
--
storage_class :: { CStorageSpec }
storage_class
  : typedef			{% withAttrs $1 $ CTypedef }
  | extern			{% withAttrs $1 $ CExtern }
  | static			{% withAttrs $1 $ CStatic }
  | auto			{% withAttrs $1 $ CAuto }
  | register			{% withAttrs $1 $ CRegister }
  | "__thread"			{% withAttrs $1 $ CThread }


-- parse C type specifier (C99 6.7.2)
--
-- This recignises a whole list of type specifiers rather than just one
-- as in the C99 grammar.
--
-- * summary:
--   [type_qualifier | basic_type_name | elaborated_type_name | tyident]{
--         1 >= basic_type_name
--     |x| 1 == elaborated_type_name
--     |x| 1 == tyident
--   }
--
type_specifier :: { [CDeclSpec] }
type_specifier
  : basic_type_specifier		{ reverse $1 }	-- Arithmetic or void
  | sue_type_specifier			{ reverse $1 }	-- Struct/Union/Enum
  | typedef_type_specifier		{ reverse $1 }	-- Typedef


basic_type_name :: { CTypeSpec }
basic_type_name
  : void			{% withAttrs $1 $ CVoidType }
  | char			{% withAttrs $1 $ CCharType }
  | short			{% withAttrs $1 $ CShortType }
  | int				{% withAttrs $1 $ CIntType }
  | long			{% withAttrs $1 $ CLongType }
  | float			{% withAttrs $1 $ CFloatType }
  | double			{% withAttrs $1 $ CDoubleType }
  | signed			{% withAttrs $1 $ CSignedType }
  | unsigned			{% withAttrs $1 $ CUnsigType }
  | "_Bool"			{% withAttrs $1 $ CBoolType }
  | "_Complex"			{% withAttrs $1 $ CComplexType }


-- A mixture of type qualifiers, storage class and basic type names in any
-- order, but containing at least one basic type name and at least one storage
-- class specifier.
--
-- * summary:
--   [type_qualifier | storage_class | basic_type_name]{
--     1 >= storage_class && 1 >= basic_type_name
--   }
--
basic_declaration_specifier :: { Reversed [CDeclSpec] }
basic_declaration_specifier
  : declaration_qualifier_list basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | basic_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | basic_declaration_specifier basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_declaration_specifier attr
  	{ $1 }


-- A mixture of type qualifiers and basic type names in any order, but
-- containing at least one basic type name.
--
-- * summary:
--   [type_qualifier | basic_type_name]{ 1 >= basic_type_name }
--
basic_type_specifier :: { Reversed [CDeclSpec] }
basic_type_specifier
  -- Arithmetic or void
  : basic_type_name
  	{ singleton (CTypeSpec $1) }

  | type_qualifier_list basic_type_name
  	{ rmap CTypeQual $1 `snoc` CTypeSpec $2 }

  | basic_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | basic_type_specifier basic_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | basic_type_specifier attr
  	{ $1 }


-- A named or anonymous struct, union or enum type along with at least one
-- storage class and any mix of type qualifiers.
-- 
-- * summary:
--   [type_qualifier | storage_class | elaborated_type_name]{ 
--     1 == elaborated_type_name && 1 >= storage_class
--   }
--
sue_declaration_specifier :: { Reversed [CDeclSpec] }
sue_declaration_specifier
  : declaration_qualifier_list elaborated_type_name
  	{ $1 `snoc` CTypeSpec $2 }

  | sue_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | sue_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | sue_declaration_specifier attr
  	{ $1 }


-- A struct, union or enum type (named or anonymous) with optional leading and
-- trailing type qualifiers.
--
-- * summary:
--   [type_qualifier] elaborated_type_name [type_qualifier]
--
sue_type_specifier :: { Reversed [CDeclSpec] }
sue_type_specifier
  -- struct/union/enum
  : elaborated_type_name
  	{ singleton (CTypeSpec $1) }

  | type_qualifier_list elaborated_type_name
  	{ rmap CTypeQual $1 `snoc` CTypeSpec $2 }

  | sue_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | sue_type_specifier attr
  	{ $1 }


-- A typedef'ed type identifier with at least one storage qualifier and any
-- number of type qualifiers
--
-- * Summary:
--   [type_qualifier | storage_class | tyident]{
--     1 == tyident && 1 >= storage_class
--   }
--
-- * Note:
--   the tyident can also be a: typeof '(' ... ')'
--
typedef_declaration_specifier :: { Reversed [CDeclSpec] }
typedef_declaration_specifier
  : typedef_type_specifier storage_class
  	{ $1 `snoc` CStorageSpec $2 }

  | declaration_qualifier_list tyident
  	{% withAttrs $1 $ \attr -> $1 `snoc` CTypeSpec (CTypeDef $2 attr) }

  | declaration_qualifier_list typeof '(' expression ')'
  	{% withAttrs $1 $ \attr -> $1 `snoc` CTypeSpec (CTypeOfExpr $4 attr) }

  | declaration_qualifier_list typeof '(' type_name ')'
  	{% withAttrs $1 $ \attr -> $1 `snoc` CTypeSpec (CTypeOfType $4 attr) }

  | typedef_declaration_specifier declaration_qualifier
  	{ $1 `snoc` $2 }

  | typedef_declaration_specifier attr
  	{ $1 }


-- typedef'ed type identifier with optional leading and trailing type qualifiers
--
-- * Summary:
--   [type_qualifier] ( tyident | typeof '('...')' ) [type_qualifier]
--
typedef_type_specifier :: { Reversed [CDeclSpec] }
typedef_type_specifier
  : tyident
  	{% withAttrs $1 $ \attr -> singleton (CTypeSpec (CTypeDef $1 attr)) }

  | typeof '(' expression ')'
  	{% withAttrs $1 $ \attr -> singleton (CTypeSpec (CTypeOfExpr $3 attr)) }

  | typeof '(' type_name ')'
  	{% withAttrs $1 $ \attr -> singleton (CTypeSpec (CTypeOfType $3 attr)) }

  | type_qualifier_list tyident
  	{% withAttrs $2 $ \attr -> rmap CTypeQual $1 `snoc` CTypeSpec (CTypeDef $2 attr) }

  | type_qualifier_list typeof '(' expression ')'
  	{% withAttrs $2 $ \attr -> rmap CTypeQual $1 `snoc` CTypeSpec (CTypeOfExpr $4 attr) }

  | type_qualifier_list typeof '(' type_name ')'
  	{% withAttrs $2 $ \attr -> rmap CTypeQual $1 `snoc` CTypeSpec (CTypeOfType $4 attr) }

  | typedef_type_specifier type_qualifier
  	{ $1 `snoc` CTypeQual $2 }

  | typedef_type_specifier attr
  	{ $1 }


-- A named or anonymous struct, union or enum type.
--
-- * summary:
--   (struct|union|enum) (identifier? '{' ... '}' | identifier)
--
elaborated_type_name :: { CTypeSpec }
elaborated_type_name
  : struct_or_union_specifier	{% withAttrs $1 $ CSUType $1 }
  | enum_specifier		{% withAttrs $1 $ CEnumType $1 }


-- parse C structure or union declaration (C99 6.7.2.1)
--
-- * summary:
--   (struct|union) (identifier? '{' ... '}' | identifier)
--
struct_or_union_specifier :: { CStructUnion }
struct_or_union_specifier
  : struct_or_union attrs_opt identifier '{' struct_declaration_list '}'
  	{% withAttrs $1 $ CStruct (unL $1) (Just $3) (reverse $5) }

  | struct_or_union attrs_opt '{' struct_declaration_list '}'
  	{% withAttrs $1 $ CStruct (unL $1) Nothing   (reverse $4) }

  | struct_or_union attrs_opt identifier
  	{% withAttrs $1 $ CStruct (unL $1) (Just $3) [] }


struct_or_union :: { Located CStructTag }
struct_or_union
  : struct			{ L CStructTag (posOf $1) }
  | union			{ L CUnionTag (posOf $1) }


struct_declaration_list :: { Reversed [CDecl] }
struct_declaration_list
  : {- empty -}						{ empty }
  | struct_declaration_list ';'				{ $1 }
  | struct_declaration_list struct_declaration		{ $1 `snoc` $2 }


-- parse C structure declaration (C99 6.7.2.1)
--
struct_declaration :: { CDecl }
struct_declaration
  : struct_declaring_list ';'
  	{ case $1 of CDecl declspecs dies attr -> CDecl declspecs (List.reverse dies) attr }

  | struct_default_declaring_list ';'
  	{ case $1 of CDecl declspecs dies attr -> CDecl declspecs (List.reverse dies) attr }

  | "__extension__" struct_declaration	{ $2 }


-- doesn't redeclare typedef
struct_default_declaring_list :: { CDecl }
struct_default_declaring_list
  : attrs_opt type_qualifier_list struct_identifier_declarator attrs_opt
  	{% withAttrs $2 $ case $3 of (d,s) -> CDecl (liftTypeQuals $2) [(d,Nothing,s)] }

  | struct_default_declaring_list ',' attrs_opt struct_identifier_declarator attrs_opt
  	{ case $1 of
            CDecl declspecs dies attr ->
              case $4 of
                (d,s) -> CDecl declspecs ((d,Nothing,s) : dies) attr }


-- * GNU extensions:
--     allow anonymous nested structures and unions
--
struct_declaring_list :: { CDecl }
struct_declaring_list
  : attrs_opt type_specifier struct_declarator attrs_opt
  	{% withAttrs $2 $ case $3 of (d,s) -> CDecl $2 [(d,Nothing,s)] }

  | struct_declaring_list ',' attrs_opt struct_declarator attrs_opt
  	{ case $1 of
            CDecl declspecs dies attr ->
              case $4 of
                (d,s) -> CDecl declspecs ((d,Nothing,s) : dies) attr }

  -- We're being far too liberal in the parsing here, we realyl want to just
  -- allow unnamed struct and union fields but we're actually allowing any
  -- unnamed struct member. Making it allow only unnamed structs or unions in
  -- the parser is far too tricky, it makes things ambiguous. So we'll have to
  -- diagnose unnamed fields that are not structs/unions in a later stage.
  | attrs_opt type_specifier
        {% withAttrs $2 $ CDecl $2 [] }


-- parse C structure declarator (C99 6.7.2.1)
--
struct_declarator :: { (Maybe CDeclr, Maybe CExpr) }
struct_declarator
  : declarator					{ (Just $1, Nothing) }
  | ':' constant_expression			{ (Nothing, Just $2) }
  | declarator ':' constant_expression		{ (Just $1, Just $3) }


struct_identifier_declarator :: { (Maybe CDeclr, Maybe CExpr) }
struct_identifier_declarator
  : identifier_declarator				{ (Just $1, Nothing) }
  | ':' constant_expression				{ (Nothing, Just $2) }
  | identifier_declarator ':' constant_expression	{ (Just $1, Just $3) }


-- parse C enumeration declaration (C99 6.7.2.2)
--
-- * summary:
--   enum (identifier? '{' ... '}' | identifier)
--
enum_specifier :: { CEnum }
enum_specifier
  : enum attrs_opt '{' enumerator_list '}'
  	{% withAttrs $1 $ CEnum Nothing   (reverse $4) }

  | enum attrs_opt '{' enumerator_list ',' '}'
  	{% withAttrs $1 $ CEnum Nothing   (reverse $4) }

  | enum attrs_opt identifier '{' enumerator_list '}'
  	{% withAttrs $1 $ CEnum (Just $3) (reverse $5) }

  | enum attrs_opt identifier '{' enumerator_list ',' '}'
  	{% withAttrs $1 $ CEnum (Just $3) (reverse $5) }

  | enum attrs_opt identifier
  	{% withAttrs $1 $ CEnum (Just $3) []           }


enumerator_list :: { Reversed [(Ident, Maybe CExpr)] }
enumerator_list
  : enumerator					{ singleton $1 }
  | enumerator_list ',' enumerator		{ $1 `snoc` $3 }


enumerator :: { (Ident, Maybe CExpr) }
enumerator
  : identifier					{ ($1, Nothing) }
  | identifier '=' constant_expression		{ ($1, Just $3) }


-- parse C type qualifier (C99 6.7.3)
--
type_qualifier :: { CTypeQual }
type_qualifier
  : const		{% withAttrs $1 $ CConstQual }
  | volatile		{% withAttrs $1 $ CVolatQual }
  | restrict		{% withAttrs $1 $ CRestrQual }
  | inline		{% withAttrs $1 $ CInlinQual }


-- parse C declarator (C99 6.7.5)
--
declarator :: { CDeclr }
declarator
  : identifier_declarator		{ $1 }
  | typedef_declarator			{ $1 }


-- Parse GNU C's asm annotations
--
asm_opt :: { () }
asm_opt
  : {- empty -}				{ () }
  | asm '(' string_literal_list ')'	{ () }


typedef_declarator :: { CDeclr }
typedef_declarator
  -- would be ambiguous as parameter
  : paren_typedef_declarator		{ $1 }
  
  -- not ambiguous as param
  | parameter_typedef_declarator	{ $1 }


parameter_typedef_declarator :: { CDeclr }
parameter_typedef_declarator
  : tyident
  	{% withAttrs $1 $ CVarDeclr (Just $1) }

  | tyident postfixing_abstract_declarator
  	{% withAttrs $1 $ \attrs -> $2 (CVarDeclr (Just $1) attrs) }

  | clean_typedef_declarator
  	{ $1 }


-- The  following have at least one '*'.
-- There is no (redundant) '(' between the '*' and the tyident.
clean_typedef_declarator :: { CDeclr }
clean_typedef_declarator
  : clean_postfix_typedef_declarator
  	{ $1 }

  | '*' parameter_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $2 }

  | '*' type_qualifier_list parameter_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $3 }

  | '*' attrs parameter_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $3 }

  | '*' attrs type_qualifier_list parameter_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) $4 }


clean_postfix_typedef_declarator :: { CDeclr }
clean_postfix_typedef_declarator
  : '(' clean_typedef_declarator ')'						{ $2 }
  | '(' attrs clean_typedef_declarator ')'					{ $3 }
  | '(' clean_typedef_declarator ')' postfixing_abstract_declarator		{ $4 $2 }
  | '(' attrs clean_typedef_declarator ')' postfixing_abstract_declarator	{ $5 $3 }


-- The following have a redundant '(' placed
-- immediately to the left of the tyident
paren_typedef_declarator :: { CDeclr }
paren_typedef_declarator
  : paren_postfix_typedef_declarator
  	{ $1 }

  -- redundant paren
  | '*' '(' simple_paren_typedef_declarator ')'
  	{% withAttrs $1 $ CPtrDeclr [] $3 }

  -- redundant paren
  | '*' type_qualifier_list '(' simple_paren_typedef_declarator ')'
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $4 }

  | '*' paren_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $2 }

  | '*' type_qualifier_list paren_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $3 }

  | '*' attrs '(' simple_paren_typedef_declarator ')'
  	{% withAttrs $1 $ CPtrDeclr [] $4 }

  -- redundant paren
  | '*' attrs type_qualifier_list '(' simple_paren_typedef_declarator ')'
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) $5 }

  | '*' attrs paren_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $3 }

  | '*' attrs type_qualifier_list paren_typedef_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) $4 }


-- redundant paren to left of tname
paren_postfix_typedef_declarator :: { CDeclr }
paren_postfix_typedef_declarator
  : '(' paren_typedef_declarator ')'
  	{ $2 }

  -- redundant paren
  | '(' simple_paren_typedef_declarator postfixing_abstract_declarator ')'
  	{ $3 $2 }

  | '(' paren_typedef_declarator ')' postfixing_abstract_declarator
  	{ $4 $2 }


-- Just a type name in any number of nested brackets
--
simple_paren_typedef_declarator :: { CDeclr }
simple_paren_typedef_declarator
  : tyident
  	{% withAttrs $1 $ CVarDeclr (Just $1) }

  | '(' simple_paren_typedef_declarator ')'
  	{ $2 }


identifier_declarator :: { CDeclr }
identifier_declarator
  : unary_identifier_declarator			{ $1 }
  | paren_identifier_declarator			{ $1 }


unary_identifier_declarator :: { CDeclr }
unary_identifier_declarator
  : postfix_identifier_declarator
  	{ $1 }

  | '*' identifier_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $2 }

  | '*' type_qualifier_list identifier_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $3 }

  | '*' attrs identifier_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $3 }

  | '*' attrs type_qualifier_list identifier_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) $4 }


postfix_identifier_declarator :: { CDeclr }
postfix_identifier_declarator
  : paren_identifier_declarator postfixing_abstract_declarator
  	{ $2 $1 }

  | '(' unary_identifier_declarator ')'
  	{ $2 }

  | '(' unary_identifier_declarator ')' postfixing_abstract_declarator
  	{ $4 $2 }

  | '(' attrs unary_identifier_declarator ')'
  	{ $3 }

  | '(' attrs unary_identifier_declarator ')' postfixing_abstract_declarator
  	{ $5 $3 }


paren_identifier_declarator :: { CDeclr }
paren_identifier_declarator
  : ident
  	{% withAttrs $1 $ CVarDeclr (Just $1) }

  | '(' paren_identifier_declarator ')'
  	{ $2 }


old_function_declarator :: { CDeclr }
old_function_declarator
  : postfix_old_function_declarator
  	{ $1 }

  | '*' old_function_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $2 }

  | '*' type_qualifier_list old_function_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $3 }


postfix_old_function_declarator :: { CDeclr }
postfix_old_function_declarator
  : paren_identifier_declarator '(' identifier_list ')'
  	{% withAttrs $2 $ CFunDeclr $1 [] False }

  | '(' old_function_declarator ')'
  	{ $2 }

  | '(' old_function_declarator ')' postfixing_abstract_declarator
  	{ $4 $2 }


type_qualifier_list :: { Reversed [CTypeQual] }
type_qualifier_list
  : type_qualifier			{ singleton $1 }
  | type_qualifier_list type_qualifier	{ $1 `snoc` $2 }
  | type_qualifier_list attr		{ $1 }


-- parse C parameter type list (C99 6.7.5)
--
parameter_type_list :: { ([CDecl], Bool) }
parameter_type_list
  : {- empty -}				{ ([], False)}
  | parameter_list			{ (reverse $1, False) }
  | parameter_list ',' "..."		{ (reverse $1, True) }


parameter_list :: { Reversed [CDecl] }
parameter_list
  : parameter_declaration				{ singleton $1 }
  | attrs parameter_declaration				{ singleton $2 }
  | parameter_list ',' attrs_opt parameter_declaration	{ $1 `snoc` $4 }


parameter_declaration :: { CDecl }
parameter_declaration
  : declaration_specifier
  	{% withAttrs $1 $ CDecl $1 [] }

  | declaration_specifier abstract_declarator
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | declaration_specifier identifier_declarator attrs_opt
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | declaration_specifier parameter_typedef_declarator attrs_opt
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | declaration_qualifier_list
  	{% withAttrs $1 $ CDecl (reverse $1) [] }

  | declaration_qualifier_list abstract_declarator
  	{% withAttrs $1 $ CDecl (reverse $1) [(Just $2, Nothing, Nothing)] }

  | declaration_qualifier_list identifier_declarator attrs_opt
  	{% withAttrs $1 $ CDecl (reverse $1) [(Just $2, Nothing, Nothing)] }

  | type_specifier
  	{% withAttrs $1 $ CDecl $1 [] }

  | type_specifier abstract_declarator
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | type_specifier identifier_declarator attrs_opt
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | type_specifier parameter_typedef_declarator attrs_opt
  	{% withAttrs $1 $ CDecl $1 [(Just $2, Nothing, Nothing)] }

  | type_qualifier_list
  	{% withAttrs $1 $ CDecl (liftTypeQuals $1) [] }

  | type_qualifier_list abstract_declarator
  	{% withAttrs $1 $ CDecl (liftTypeQuals $1) [(Just $2, Nothing, Nothing)] }

  | type_qualifier_list identifier_declarator attrs_opt
  	{% withAttrs $1 $ CDecl (liftTypeQuals $1) [(Just $2, Nothing, Nothing)] }


identifier_list :: { Reversed [Ident] }
identifier_list
  : ident				{ singleton $1 }
  | identifier_list ',' ident		{ $1 `snoc` $3 }


-- parse C type name (C99 6.7.6)
--
type_name :: { CDecl }
type_name
  : attrs_opt type_specifier
  	{% withAttrs $2 $ CDecl $2 [] }

  | attrs_opt type_specifier abstract_declarator
  	{% withAttrs $2 $ CDecl $2 [(Just $3, Nothing, Nothing)] }

  | attrs_opt type_qualifier_list
  	{% withAttrs $2 $ CDecl (liftTypeQuals $2) [] }

  | attrs_opt type_qualifier_list abstract_declarator
  	{% withAttrs $2 $ CDecl (liftTypeQuals $2) [(Just $3, Nothing, Nothing)] }


-- parse C abstract declarator (C99 6.7.6)
--
abstract_declarator :: { CDeclr }
abstract_declarator
  : unary_abstract_declarator			{ $1 }
  | postfix_abstract_declarator			{ $1 }
  | postfixing_abstract_declarator attrs_opt	{ $1 emptyDeclr }


postfixing_abstract_declarator :: { CDeclr -> CDeclr }
postfixing_abstract_declarator
  : array_abstract_declarator
  	{ $1 }

  | '(' parameter_type_list ')'
  	{% withAttrs $1 $ \attrs declr -> case $2 of
             (params, variadic) -> CFunDeclr declr params variadic attrs }


-- * Note that we recognise but ignore the C99 static keyword (see C99 6.7.5.3)
--
-- * We do not distinguish in the AST between incomplete array types and
-- complete variable length arrays ([ '*' ] means the latter). (see C99 6.7.5.2)
--
array_abstract_declarator :: { CDeclr -> CDeclr }
array_abstract_declarator
  : postfix_array_abstract_declarator
  	{ $1 }

  | array_abstract_declarator postfix_array_abstract_declarator
  	{ \decl -> $2 ($1 decl) }


postfix_array_abstract_declarator :: { CDeclr -> CDeclr }
postfix_array_abstract_declarator
  : '[' assignment_expression_opt ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr [] $2 attrs }

  | '[' type_qualifier_list assignment_expression_opt ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr (reverse $2) $3 attrs }

  | '[' static assignment_expression ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr [] (Just $3) attrs }

  | '[' static type_qualifier_list assignment_expression ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr (reverse $3) (Just $4) attrs }

  | '[' type_qualifier_list static assignment_expression ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr (reverse $2) (Just $4) attrs }

  | '[' '*' ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr [] Nothing attrs }

  | '[' type_qualifier_list '*' ']'
  	{% withAttrs $1 $ \attrs declr -> CArrDeclr declr (reverse $2) Nothing attrs }


unary_abstract_declarator :: { CDeclr }
unary_abstract_declarator
  : '*'
  	{% withAttrs $1 $ CPtrDeclr [] emptyDeclr }

  | '*' type_qualifier_list
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) emptyDeclr }

  | '*' abstract_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $2 }

  | '*' type_qualifier_list abstract_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $2) $3 }

  | '*' attrs
  	{% withAttrs $1 $ CPtrDeclr [] emptyDeclr }

  | '*' attrs type_qualifier_list
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) emptyDeclr }

  | '*' attrs abstract_declarator
  	{% withAttrs $1 $ CPtrDeclr [] $3 }

  | '*' attrs type_qualifier_list abstract_declarator
  	{% withAttrs $1 $ CPtrDeclr (reverse $3) $4 }


postfix_abstract_declarator :: { CDeclr }
postfix_abstract_declarator
  : '(' unary_abstract_declarator ')'					{ $2 }
  | '(' postfix_abstract_declarator ')'					{ $2 }
  | '(' postfixing_abstract_declarator ')'				{ $2 emptyDeclr }
  | '(' unary_abstract_declarator ')' postfixing_abstract_declarator	{ $4 $2 }
  | '(' attrs unary_abstract_declarator ')'					{ $3 }
  | '(' attrs postfix_abstract_declarator ')'					{ $3 }
  | '(' attrs postfixing_abstract_declarator ')'				{ $3 emptyDeclr }
  | '(' attrs unary_abstract_declarator ')' postfixing_abstract_declarator	{ $5 $3 }
  | postfix_abstract_declarator attr						{ $1 }


-- parse C initializer (C99 6.7.8)
--
initializer :: { CInit }
initializer
  : assignment_expression		{% withAttrs $1 $ CInitExpr $1 }
  | '{' initializer_list '}'		{% withAttrs $1 $ CInitList (reverse $2) }
  | '{' initializer_list ',' '}'	{% withAttrs $1 $ CInitList (reverse $2) }


initializer_opt :: { Maybe CInit }
initializer_opt
  : {- empty -}			{ Nothing }
  | '=' initializer		{ Just $2 }


initializer_list :: { Reversed CInitList }
initializer_list
  : {- empty -}						{ empty }
  | initializer						{ singleton ([],$1) }
  | designation initializer				{ singleton ($1,$2) }
  | initializer_list ',' initializer			{ $1 `snoc` ([],$3) }
  | initializer_list ',' designation initializer	{ $1 `snoc` ($3,$4) }


-- designation
--
-- * GNU extensions:
--     old style member designation: 'ident :'
--     array range designation
--
designation :: { [CDesignator] }
designation
  : designator_list '='		{ reverse $1 }
  | identifier ':'		{% withAttrs $1 $ \at -> [CMemberDesig $1 at] }
  | array_designator		{ [$1] }


designator_list :: { Reversed [CDesignator] }
designator_list
 : designator				{ singleton $1 }
 | designator_list designator		{ $1 `snoc` $2 }


designator :: { CDesignator }
designator
  : '[' constant_expression ']'		{% withAttrs $1 $ CArrDesig $2 }
  | '.' identifier			{% withAttrs $1 $ CMemberDesig $2 }
  | array_designator			{ $1 }


array_designator :: { CDesignator }
array_designator
  : '[' constant_expression "..." constant_expression ']'
  	{% withAttrs $1 $ CRangeDesig $2 $4 }


-- parse C primary expression (C99 6.5.1)
--
-- We cannot use a typedef name as a variable
--
-- * GNU extensions:
--     allow a compound statement as an expression
--     various __builtin_* forms that take type parameters
--
primary_expression :: { CExpr }
primary_expression
  : ident		{% withAttrs $1 $ CVar $1 }
  | constant	  	{% withAttrs $1 $ CConst $1 }
  | string_literal	{% withAttrs $1 $ CConst $1 }
  | '(' expression ')'	{ $2 }
  | '(' compound_statement ')'
  	{% withAttrs $1 $ CStatExpr $2 }

  | "__builtin_va_arg" '(' assignment_expression ',' type_name ')'
  	{% withAttrs $1 CBuiltinExpr }

  | "__builtin_offsetof" '(' type_name ',' offsetof_member_designator ')'
  	{% withAttrs $1 CBuiltinExpr }

  | "__builtin_types_compatible_p" '(' type_name ',' type_name ')'
  	{% withAttrs $1 CBuiltinExpr }


offsetof_member_designator :: { () }
offsetof_member_designator
  : ident						{ () }
  | offsetof_member_designator '.' ident		{ () }
  | offsetof_member_designator '[' expression ']'	{ () }


--parse C postfix expression (C99 6.5.2)
--
postfix_expression :: { CExpr }
postfix_expression
  : primary_expression
  	{ $1 }

  | postfix_expression '[' expression ']'
  	{% withAttrs $2 $ CIndex $1 $3 }

  | postfix_expression '(' ')'
  	{% withAttrs $2 $ CCall $1 [] }

  | postfix_expression '(' argument_expression_list ')'
  	{% withAttrs $2 $ CCall $1 (reverse $3) }

  | postfix_expression '.' identifier
  	{% withAttrs $2 $ CMember $1 $3 False }

  | postfix_expression "->" identifier
  	{% withAttrs $2 $ CMember $1 $3 True }

  | postfix_expression "++"
  	{% withAttrs $2 $ CUnary CPostIncOp $1 }

  | postfix_expression "--"
  	{% withAttrs $2 $ CUnary CPostDecOp $1 }

  | '(' type_name ')' '{' initializer_list '}'
  	{% withAttrs $4 $ CCompoundLit $2 (reverse $5) }

  | '(' type_name ')' '{' initializer_list ',' '}'
  	{% withAttrs $4 $ CCompoundLit $2 (reverse $5) }


argument_expression_list :: { Reversed [CExpr] }
argument_expression_list
  : assignment_expression				{ singleton $1 }
  | argument_expression_list ',' assignment_expression	{ $1 `snoc` $3 }


-- parse C unary expression (C99 6.5.3)
--
-- * GNU extensions:
--     'alignof' expression or type
--     '__extension__' to suppress warnings about extensions
--     allow taking address of a label with: && label
--
unary_expression :: { CExpr }
unary_expression
  : postfix_expression			{ $1 }
  | "++" unary_expression		{% withAttrs $1 $ CUnary CPreIncOp $2 }
  | "--" unary_expression		{% withAttrs $1 $ CUnary CPreDecOp $2 }
  | "__extension__" cast_expression	{ $2 }
  | unary_operator cast_expression	{% withAttrs $1 $ CUnary (unL $1) $2 }
  | sizeof unary_expression		{% withAttrs $1 $ CSizeofExpr $2 }
  | sizeof '(' type_name ')'		{% withAttrs $1 $ CSizeofType $3 }
  | alignof unary_expression		{% withAttrs $1 $ CAlignofExpr $2 }
  | alignof '(' type_name ')'		{% withAttrs $1 $ CAlignofType $3 }
  | "&&" identifier			{% withAttrs $1 $ CLabAddrExpr $2 }


unary_operator :: { Located CUnaryOp }
unary_operator
  : '&'		{ L CAdrOp  (posOf $1) }
  | '*'		{ L CIndOp  (posOf $1) }
  | '+'		{ L CPlusOp (posOf $1) }
  | '-'		{ L CMinOp  (posOf $1) }
  | '~'		{ L CCompOp (posOf $1) }
  | '!'		{ L CNegOp  (posOf $1) }


-- parse C cast expression (C99 6.5.4)
--
cast_expression :: { CExpr }
cast_expression
  : unary_expression			{ $1 }
  | '(' type_name ')' cast_expression	{% withAttrs $1 $ CCast $2 $4 }


-- parse C multiplicative expression (C99 6.5.5)
--
multiplicative_expression :: { CExpr }
multiplicative_expression
  : cast_expression
  	{ $1 }

  | multiplicative_expression '*' cast_expression
  	{% withAttrs $2 $ CBinary CMulOp $1 $3 }

  | multiplicative_expression '/' cast_expression
  	{% withAttrs $2 $ CBinary CDivOp $1 $3 }

  | multiplicative_expression '%' cast_expression
  	{% withAttrs $2 $ CBinary CRmdOp $1 $3 }


-- parse C additive expression (C99 6.5.6)
--
additive_expression :: { CExpr }
additive_expression
  : multiplicative_expression
  	{ $1 }

  | additive_expression '+' multiplicative_expression
  	{% withAttrs $2 $ CBinary CAddOp $1 $3 }

  | additive_expression '-' multiplicative_expression
  	{% withAttrs $2 $ CBinary CSubOp $1 $3 }


-- parse C shift expression (C99 6.5.7)
--
shift_expression :: { CExpr }
shift_expression
  : additive_expression
  	{ $1 }

  | shift_expression "<<" additive_expression
  	{% withAttrs $2 $ CBinary CShlOp $1 $3 }

  | shift_expression ">>" additive_expression
  	{% withAttrs $2 $ CBinary CShrOp $1 $3 }


-- parse C relational expression (C99 6.5.8)
--
relational_expression :: { CExpr }
relational_expression
  : shift_expression
  	{ $1 }

  | relational_expression '<' shift_expression
  	{% withAttrs $2 $ CBinary CLeOp $1 $3 }

  | relational_expression '>' shift_expression
  	{% withAttrs $2 $ CBinary CGrOp $1 $3 }

  | relational_expression "<=" shift_expression
  	{% withAttrs $2 $ CBinary CLeqOp $1 $3 }

  | relational_expression ">=" shift_expression
  	{% withAttrs $2 $ CBinary CGeqOp $1 $3 }


-- parse C equality expression (C99 6.5.9)
--
equality_expression :: { CExpr }
equality_expression
  : relational_expression
  	{ $1 }

  | equality_expression "==" relational_expression
  	{% withAttrs $2 $ CBinary CEqOp  $1 $3 }

  | equality_expression "!=" relational_expression
  	{% withAttrs $2 $ CBinary CNeqOp $1 $3 }


-- parse C bitwise and expression (C99 6.5.10)
--
and_expression :: { CExpr }
and_expression
  : equality_expression
  	{ $1 }

  | and_expression '&' equality_expression
  	{% withAttrs $2 $ CBinary CAndOp $1 $3 }


-- parse C bitwise exclusive or expression (C99 6.5.11)
--
exclusive_or_expression :: { CExpr }
exclusive_or_expression
  : and_expression
  	{ $1 }

  | exclusive_or_expression '^' and_expression
  	{% withAttrs $2 $ CBinary CXorOp $1 $3 }


-- parse C bitwise or expression (C99 6.5.12)
--
inclusive_or_expression :: { CExpr }
inclusive_or_expression
  : exclusive_or_expression
  	{ $1 }

  | inclusive_or_expression '|' exclusive_or_expression
  	{% withAttrs $2 $ CBinary COrOp $1 $3 }


-- parse C logical and expression (C99 6.5.13)
--
logical_and_expression :: { CExpr }
logical_and_expression
  : inclusive_or_expression
  	{ $1 }

  | logical_and_expression "&&" inclusive_or_expression
  	{% withAttrs $2 $ CBinary CLndOp $1 $3 }


-- parse C logical or expression (C99 6.5.14)
--
logical_or_expression :: { CExpr }
logical_or_expression
  : logical_and_expression
  	{ $1 }

  | logical_or_expression "||" logical_and_expression
  	{% withAttrs $2 $ CBinary CLorOp $1 $3 }


-- parse C conditional expression (C99 6.5.15)
--
-- * GNU extensions:
--     omitting the `then' part
--
conditional_expression :: { CExpr }
conditional_expression
  : logical_or_expression
  	{ $1 }

  | logical_or_expression '?' expression ':' conditional_expression
  	{% withAttrs $2 $ CCond $1 (Just $3) $5 }

  | logical_or_expression '?' ':' conditional_expression
  	{% withAttrs $2 $ CCond $1 Nothing $4 }


-- parse C assignment expression (C99 6.5.16)
--
assignment_expression :: { CExpr }
assignment_expression
  : conditional_expression
  	{ $1 }

  | unary_expression assignment_operator assignment_expression
  	{% withAttrs $2 $ CAssign (unL $2) $1 $3 }


assignment_operator :: { Located CAssignOp }
assignment_operator
  : '='			{ L CAssignOp (posOf $1) }
  | "*="		{ L CMulAssOp (posOf $1) }
  | "/="		{ L CDivAssOp (posOf $1) }
  | "%="		{ L CRmdAssOp (posOf $1) }
  | "+="		{ L CAddAssOp (posOf $1) }
  | "-="		{ L CSubAssOp (posOf $1) }
  | "<<="		{ L CShlAssOp (posOf $1) }
  | ">>="		{ L CShrAssOp (posOf $1) }
  | "&="		{ L CAndAssOp (posOf $1) }
  | "^="		{ L CXorAssOp (posOf $1) }
  | "|="		{ L COrAssOp  (posOf $1) }


-- parse C expression (C99 6.5.17)
--
expression :: { CExpr }
expression
  : assignment_expression
  	{ $1 }

  | assignment_expression ',' comma_expression
  	{% let es = reverse $3 in withAttrs es $ CComma ($1:es) }


comma_expression :: { Reversed [CExpr] }
comma_expression
  : assignment_expression			{ singleton $1 }
  | comma_expression ',' assignment_expression	{ $1 `snoc` $3 }


-- The following was used for clarity
expression_opt :: { Maybe CExpr }
expression_opt
  : {- empty -}		{ Nothing }
  | expression		{ Just $1 }


-- The following was used for clarity
assignment_expression_opt :: { Maybe CExpr }
assignment_expression_opt
  : {- empty -}				{ Nothing }
  | assignment_expression		{ Just $1 }


-- parse C constant expression (C99 6.6)
--
constant_expression :: { CExpr }
constant_expression
  : conditional_expression			{ $1 }


-- parse C constants
--
constant :: { CConst }
constant
  : cint	{% withAttrs $1 $ case $1 of CTokILit _ i -> CIntConst i }
  | cchar	{% withAttrs $1 $ case $1 of CTokCLit _ c -> CCharConst c }
  | cfloat	{% withAttrs $1 $ case $1 of CTokFLit _ f -> CFloatConst f }


string_literal :: { CConst }
string_literal
  : cstr
  	{% withAttrs $1 $ case $1 of CTokSLit _ s -> CStrConst s }

  | cstr string_literal_list
  	{% withAttrs $1 $ case $1 of CTokSLit _ s -> CStrConst (concat (s : reverse $2)) }


string_literal_list :: { Reversed [String] }
string_literal_list
  : cstr			{ case $1 of CTokSLit _ s -> singleton s }
  | string_literal_list cstr	{ case $2 of CTokSLit _ s -> $1 `snoc` s }


identifier :: { Ident }
identifier
  : ident		{ $1 }
  | tyident		{ $1 }



-- parse GNU C attribute annotation (junking the result)
--
attrs_opt ::	{ () }
attrs_opt
  : {- empty -}						{ () }
  | attrs_opt attr					{ () }


attrs :: { () }
attrs
  : attr						{ () }
  | attrs attr	{ () }


attr :: { () }
attr
  : "__attribute__" '(' '(' attribute_list ')' ')'	{ () }


attribute_list :: { () }
  : attribute						{ () } 
  | attribute_list ',' attribute			{ () } 


attribute :: { () }
attribute
  : {- empty -}						{ () }
  | ident						{ () }
  | const						{ () }
  | ident '(' attribute_params ')'			{ () }
  | ident '(' ')'					{ () }


attribute_params :: { () }
attribute_params
  : constant_expression					{ () }
  | attribute_params ',' constant_expression		{ () }


{

infixr 5 `snoc`

-- Due to the way the grammar is constructed we very often have to build lists
-- in reverse. To make sure we do this consistently and correctly we have a
-- newtype to wrap the reversed style of list:
--
newtype Reversed a = Reversed a

empty :: Reversed [a]
empty = Reversed []

singleton :: a -> Reversed [a]
singleton x = Reversed [x]

snoc :: Reversed [a] -> a -> Reversed [a]
snoc (Reversed xs) x = Reversed (x : xs)

rmap :: (a -> b) -> Reversed [a] -> Reversed [b]
rmap f (Reversed xs) = Reversed (map f xs)

reverse :: Reversed [a] -> [a]
reverse (Reversed xs) = List.reverse xs

-- We occasionally need things to have a location when they don't naturally
-- have one built in as tokens and most AST elements do.
--
data Located a = L !a !Position

unL :: Located a -> a
unL (L a pos) = a

instance Pos (Located a) where
  posOf (L _ pos) = pos

{-# INLINE withAttrs #-}
withAttrs :: Pos node => node -> (Attrs -> a) -> P a
withAttrs node mkAttributedNode = do
  name <- getNewName
  let attrs = newAttrs (posOf node) name
  attrs `seq` return (mkAttributedNode attrs)

-- this functions gets used repeatedly so take them out of line:
--
liftTypeQuals :: Reversed [CTypeQual] -> [CDeclSpec]
liftTypeQuals (Reversed xs) = revmap [] xs
  where revmap a []     = a
        revmap a (x:xs) = revmap (CTypeQual x : a) xs


-- convenient instance, the position of a list of things is the position of
-- the first thing in the list
--
instance Pos a => Pos [a] where
  posOf (x:_) = posOf x

instance Pos a => Pos (Reversed a) where
  posOf (Reversed x) = posOf x

emptyDeclr = CVarDeclr Nothing (newAttrsOnlyPos nopos)

-- Take the identifiers and use them to update the typedef'ed identifier set
-- if the decl is defining a typedef then we add it to the set,
-- if it's a var decl then that shadows typedefed identifiers
--
doDeclIdent :: [CDeclSpec] -> CDeclr -> P ()
doDeclIdent declspecs declr =
  case getCDeclrIdent declr of
    Nothing -> return ()
    Just ident | any isTypeDef declspecs -> addTypedef ident
               | otherwise               -> shadowTypedef ident

  where isTypeDef (CStorageSpec (CTypedef _)) = True
        isTypeDef _                           = False

doFuncParamDeclIdent :: CDeclr -> P ()
doFuncParamDeclIdent (CFunDeclr _ params _ _) =
  sequence_
    [ case getCDeclrIdent declr of
        Nothing -> return ()
        Just ident -> shadowTypedef ident
    | CDecl _ dle _ <- params
    , (Just declr, _, _) <- dle ]
doFuncParamDeclIdent (CPtrDeclr _ declr _ ) = doFuncParamDeclIdent declr
doFuncParamDeclIdent _ = return ()

-- extract all identifiers
getCDeclrIdent :: CDeclr -> Maybe Ident
getCDeclrIdent (CVarDeclr optIde    _) = optIde
getCDeclrIdent (CPtrDeclr _ declr   _) = getCDeclrIdent declr
getCDeclrIdent (CArrDeclr declr _ _ _) = getCDeclrIdent declr
getCDeclrIdent (CFunDeclr declr _ _ _) = getCDeclrIdent declr


happyError :: P a
happyError = parseError

parseC :: String -> Position -> PreCST s s' CHeader
parseC input initialPosition  = do
  nameSupply <- getNameSupply
  let ns = names nameSupply
  case execParser header input
                  initialPosition (map fst builtinTypeNames) ns of
    Left header -> return header
    Right (message, position) -> raiseFatal "Error in C header file."
                                            position message
}
