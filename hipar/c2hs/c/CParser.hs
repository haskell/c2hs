--  C -> Haskell Compiler: Parser for C Header Files
--
--  Author : Manuel M. T. Chakravarty
--  Created: 7 March 99
--
--  Version $Revision: 1.13 $ from $Date: 2001/02/12 06:34:39 $
--
--  Copyright (c) [1999.2001] Manuel M. T. Chakravarty
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
--  Parser for C header files, which have already been run through the C
--  preprocessor.  
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The parser recognizes all of ANCI C except function bodies.  The parser
--  combinators follow K&R Appendix A, but we make use of the richer grammar
--  constructs provided by `Parsers'.  It supports the C99 `restrict'
--  extension.
--
--  Comments:
--
--  * Subtrees representing empty declarators of the form `CVarDeclr Nothing
--    at' have *no* valid attribute handle in `at' (only a `newAttrsOnlyPos
--    nopos').
--
--  * Details on the C99 restrict extension are at:
--    <http://www.lysator.liu.se/c/restrict.html>.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.
--
--  Supported GNU C extensions:
--
--  * We also recognize GNU C `__attribute__' annotations (however, they are
--    not entered into the structure tree, but ignored).  More specifically, 
--
--      __attribute__ (( <attr> ))
--
--    may occur as a prefix and/or suffix of a `declarator', where <attr>
--    is either just an identifier or an identifier followed by a
--    comma-separated list of arguments, which may be numbers, strings, and
--    identifiers. 
--
--  * We also recognize GNU C `__extension__' annotations (however, they are
--    not entered into the structure tree, but ignored).  More specifically, 
--
--      __extension__
--
--    may occur in a specifier list.
--
--  * There may be a `,' behind the last element of a enum.
--
--  * Builtin type names are imported from `CBuiltin'.
--
--- TODO ----------------------------------------------------------------------
--
--  * Is the token transformation (`CTokIdent' -> `CTokTypeName') in
--    `parseCHeader' a performance bottleneck?  Doesn't seem so...
--

module CParser (parseC)
where

import Maybe      (catMaybes)

import Common	  (Position, Pos(..), nopos)
import Utils      (Tag(tag))
import UNames     (Name, NameSupply, names)
import Idents     (Ident)
import Attributes (Attrs, newAttrs, newAttrsOnlyPos)

import Parsers    (Token, Parser, empty, token, skip, (<|>), (*$>), (*>), ($>),
		   action, meta, opt, (-*>), (*->), many, list, many1, list1,
		   sep, seplist, sep1, seplist1, execParser)

import C2HSState  (CST, raise, getNameSupply)
import CLexer     (CToken(..), GnuCTok(..), lexC)
import CAST       (CHeader(..), CExtDecl, CDecl(..), CDeclSpec(..),
		   CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
		   CStructUnion(..), CStructTag(..), CEnum(..), CDeclr(..),
		   CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..),
		   CUnaryOp(..), CConst (..))
import CBuiltin   (builtinTypeNames)


infixl 3 `actionAttrs`, `opAction`


-- preliminaries
-- -------------

-- for the lexer
--
instance Token CToken

instance Eq CToken where
  t1 == t2 = tag t1 == tag t2

instance Ord CToken where
  t1 <= t2 = tag t1 <= tag t2

-- local parser instance
--
type CParser r = Parser [Name] CToken r

-- local variant of the action operator that generates an attribute identifier
--
-- Usage pattern:
--
--   <parser combinators>
--   `actionAttrs`
--     <position function> $
--     <result function>
--
--  <position function> computes the position attribute of the resulting
--    structure tree from the value returned by the <parser combinators>
--  <result function> computes the resulting structure tree from the value
--    returned by the <parser combinators> and an attribute handle (of type
--    `Attrs')
--
actionAttrs :: CParser s -> (s -> Position) -> (s -> Attrs -> r) -> CParser r
actionAttrs p getPos f = ((\s -> (s, getPos s)) $> p) `actionAttrs0` f 
  where
    p `actionAttrs0` f  = p *> meta getName `action` allocAttrs f
      where
	getName (name:names) = (names, name)

	allocAttrs f ((x, pos), name) = f x (newAttrs pos name)

-- action for operator symbols
--
opAction        :: CParser Position -> a -> CParser (a, Attrs)
p `opAction` op  = p `actionAttrs` id $ \_ at -> (op, at)

-- given a token constructor, yield a parser accepting such tokens
--
ctoken     :: (Position -> CToken) -> CParser Position
ctoken con  = token (con nopos) `action` posOf

ctoken_     :: (Position -> CToken) -> CParser ()
ctoken_ con  = skip (con nopos)

-- given a token constructor, yield a parser accepting such tokens and
-- yielding an attribute handle
--
ctokenAttrs     :: (Position -> CToken) -> CParser Attrs
ctokenAttrs con  = token (con nopos) `actionAttrs` posOf $ \_ at -> at

-- given a token and a corresponding structure node, yield a parser doing the
-- conversion 
--
tokenTo           :: (Position -> CToken) -> a -> CParser a
tok `tokenTo` con  = ctoken_ tok
		     `action` \_ -> con

-- given a token and a corresponding structure node, yield a parser doing the
-- conversion and also returning the token position
--
tokenToPos           :: (Position -> CToken) -> a -> CParser (a, Position)
tok `tokenToPos` con  = ctoken tok
		        `action` \pos -> (con, pos)

-- given a token and a corresponding structure node constructor, which
-- requires a attribute handle, yield a parser doing the conversion
--
tokenToAttrs           :: (Position -> CToken) -> (Attrs -> a) -> CParser a
tok `tokenToAttrs` con  = ctoken tok
			  `actionAttrs`
			    id $
			    \_ at -> con at

-- optional phrase yielding a flag
--
optBool   :: CParser a -> CParser Bool
optBool p  = (const True $> p) `opt` False

-- optional phrase with result wrapped into `Maybe'
--
optMaybe   :: CParser a -> CParser (Maybe a)
optMaybe p  = (Just $> p) `opt` Nothing

-- accept an identifier
--
cid :: CParser Ident
cid  = token (CTokIdent nopos undefined) `action` \(CTokIdent _ ide) -> ide

-- accept an identifier that is a typedef-name
--
typedefName :: CParser Ident
typedefName  = token (CTokTypeName nopos undefined) 
	       `action` \(CTokTypeName _ ide) -> ide

-- accept an integer
--
cint :: CParser (Integer, Position)
cint  = token (CTokILit nopos undefined) 
	`action` \(CTokILit pos i) -> (i, pos)

-- accept a char
--
cchar :: CParser (Char, Position)
cchar  = token (CTokCLit nopos undefined) 
	 `action` \(CTokCLit pos c) -> (c, pos)

-- accept a float
--
cfloat :: CParser (String, Position)
cfloat  = token (CTokFLit nopos undefined) 
	  `action` \(CTokFLit pos f) -> (f, pos)

-- accept a string
--
cstr :: CParser (String, Position)
cstr  = token (CTokSLit nopos undefined) 
	`action` \(CTokSLit pos s) -> (s, pos)

-- accept ";"
--
semic :: CParser Position
semic = ctoken CTokSemic

semic_ = ctoken_ CTokSemic

-- accept ","
--
comma :: CParser Position
comma = ctoken CTokComma

comma_ = ctoken_ CTokComma


-- the grammar
-- -----------

-- parse a complete C header file (K&R A10)
--
-- * the given position is that of the first token
--
-- * without any extra provisions, the C grammar is not LL(1) (nor is it
--   LALR(1)), as it is impossible to distinguish between `typedef-name' and
--   the identifiers that are part of `declarator's; it is thus necessary to
--   mark `typedef'ed type names specially; we do so by altering the remaining 
--   token stream after each typedef declaration (converting `CTokIdent'
--   tokens into corresponding `CTokTypeDef' tokens); this requires to parse a
--   declaration at a time 
--
parseCHeader            :: Position -> [CToken] -> CST s CHeader
parseCHeader pos tokens  = 
  do
    nameSupply <- getNameSupply
    let name          = (head . names) nameSupply
	at            = newAttrs pos name
	predefTypeIds = map fst builtinTypeNames
    decls <- parseCExtDeclList (morphTypeNames predefTypeIds tokens)
    return (CHeader decls at)
  where
    -- the set contains all identifiers that were turned into a
    -- typedef-name by a `typedef' declaration
    --
    parseCExtDeclList      :: [CToken] -> CST s [CExtDecl]
    parseCExtDeclList toks  = 
      do
	nameSupply <- getNameSupply
	let ns                  = names nameSupply
	    (decl, errs, toks') = execParser parseCExtDecl ns toks
	mapM raise errs
	--
	-- raise the errors first, in case any of them is fatal
	--
	let tdefNames   = getTDefNames decl
	    morphedToks = if null tdefNames then toks'	-- as `toks'' is *long*
			  else morphTypeNames tdefNames toks'
	if null morphedToks
	  then return [decl]
	  else do
	    decls <- parseCExtDeclList morphedToks
	    return (decl:decls)

    -- extract all identifiers turned into `typedef-name's
    --
    getTDefNames :: CDecl -> [Ident]
    getTDefNames (CDecl specs declrs _)
      | isTypedef = catMaybes [declrToOptIdent declr 
			      | (Just declr, _, _) <- declrs]
      | otherwise = []
      where
        isTypedef = (not . null) [()| CStorageSpec (CTypedef _) <- specs]

	declrToOptIdent :: CDeclr -> Maybe Ident
	declrToOptIdent (CVarDeclr optIde    _) = optIde
	declrToOptIdent (CPtrDeclr _ declr   _) = declrToOptIdent declr
	declrToOptIdent (CArrDeclr declr _   _) = declrToOptIdent declr
	declrToOptIdent (CFunDeclr declr _ _ _) = declrToOptIdent declr

    -- convert all identifier tokens mentioned in the first arguments into
    -- typedef-name tokens
    --
    morphTypeNames :: [Ident] -> [CToken] -> [CToken]
    morphTypeNames _     [] = []
    morphTypeNames tides (tok@(CTokIdent pos ide):toks) 
      | ide `elem` tides    = CTokTypeName pos ide : morphTypeNames tides toks
      | otherwise           = tok : morphTypeNames tides toks
    morphTypeNames tides (tok                    :toks) 
			    = tok : morphTypeNames tides toks

-- parse external C declaration (K&R A10)
--
-- * The omission of the variant of "function definitions" means that we can
--   only handle headers.
--
parseCExtDecl :: CParser CExtDecl
parseCExtDecl  = parseCDecl

-- parse C declaration (K&R A8)
--
parseCDecl :: CParser CDecl
parseCDecl  = 
  ctoken_ (CTokGnuC GnuCExtTok) `opt` ()        -- ignore GCC's __extension__
  -*> list parseCDeclSpec *> seplist comma_ parseCInitDecl *-> semic_
  `actionAttrs`
    (\(specs, declrs) -> 
      head (map posOf specs ++ map (posOf . fst) declrs ++ [nopos])) $
    \(specs, declrs) at -> let declrs' = [(Just declr, init, Nothing) 
				         | (declr, init) <- declrs]
			   in
			   CDecl specs declrs' at

-- parse C declaration specifier (K&R A8)
--
parseCDeclSpec :: CParser CDeclSpec
parseCDeclSpec  =     parseCStorageSpec 
		      `action` CStorageSpec
		  <|> 
		      parseCTypeSpec 
		      `action` CTypeSpec
		  <|> 
		      parseCTypeQual
		      `action` CTypeQual

-- parse C init declarator (K&R A8)
--
parseCInitDecl :: CParser (CDeclr, Maybe CInit)
parseCInitDecl  = parseCDeclr *> optMaybe (ctoken_ CTokAssign -*> parseCInit)

-- parse C storage class specifier (K&R A8.1)
--
parseCStorageSpec :: CParser CStorageSpec
parseCStorageSpec  =     CTokStatic  `tokenToAttrs` CStatic
		     <|> CTokExtern  `tokenToAttrs` CExtern
		     <|> CTokTypedef `tokenToAttrs` CTypedef

-- parse C type specifier (K&R A8.2)
--
parseCTypeSpec :: CParser CTypeSpec
parseCTypeSpec  =     CTokVoid     `tokenToAttrs` CVoidType
		  <|> CTokChar     `tokenToAttrs` CCharType
		  <|> CTokShort    `tokenToAttrs` CShortType
		  <|> CTokInt      `tokenToAttrs` CIntType
		  <|> CTokLong     `tokenToAttrs` CLongType
		  <|> CTokFloat    `tokenToAttrs` CFloatType
		  <|> CTokDouble   `tokenToAttrs` CDoubleType
		  <|> CTokSigned   `tokenToAttrs` CSignedType
		  <|> CTokUnsigned `tokenToAttrs` CUnsigType
		  <|> (parseCStructUnion
		       `actionAttrs`
		         posOf $
			 CSUType)
		  <|> (parseCEnum
		       `actionAttrs`
		         posOf $
			 CEnumType)
		  <|> (typedefName
		       `actionAttrs`
		         posOf $
			 CTypeDef)

-- parse C type qualifier (K&R A8.2)
--
-- * plus `restrict' from C99
--
parseCTypeQual :: CParser CTypeQual
parseCTypeQual  =     CTokConst    `tokenToAttrs` CConstQual
		  <|> CTokVolatile `tokenToAttrs` CVolatQual
		  <|> CTokRestrict `tokenToAttrs` CRestrQual

-- parse C structure of union declaration (K&R A8.3)
--
parseCStructUnion :: CParser CStructUnion
parseCStructUnion  = 
      (parseCStructTag *> optMaybe cid *> parseCStructDeclList
       `actionAttrs`
	 (\((tag, _), _) -> snd tag) $
	 \((tag, ide), decls) -> CStruct (fst tag) ide decls)
  <|> (parseCStructTag *> cid
       `actionAttrs`
	 (\(tag, _) -> snd tag) $
	 \(tag, ide) -> CStruct (fst tag) (Just ide) [])
  where
    parseCStructTag =     CTokStruct `tokenToPos` CStructTag
		      <|> CTokUnion  `tokenToPos` CUnionTag

    parseCStructDeclList = ctoken_ CTokLBrace -*>
			   list1 (parseCStructDecl) *->
			   ctoken_ CTokRBrace

-- parse C structure declaration (K&R A8.3)
--
parseCStructDecl :: CParser CDecl
parseCStructDecl  =
  list parseCSpecQual *> seplist comma_ parseCStructDeclr *-> semic_
  `actionAttrs`
    (\(specs, declrs) -> 
      head (map posOf specs ++ map declrsPos declrs ++ [nopos])) $
    \(specs, declrs) at -> let declrs' = [(declr, Nothing, size) 
				         | (declr, size) <- declrs]
			   in
			   CDecl specs declrs' at
  where
    declrsPos (Just declr, _        ) = posOf declr
    declrsPos (Nothing   , Just expr) = posOf expr
    declrsPos _			      = nopos

-- parse C specifier qualifier (K&R A8.3)
--
parseCSpecQual :: CParser CDeclSpec
parseCSpecQual  =     parseCTypeSpec 
		      `action` CTypeSpec
		  <|> 
		      parseCTypeQual
		      `action` CTypeQual

-- parse C structure declarator (K&R A8.3)
--
parseCStructDeclr :: CParser (Maybe CDeclr, Maybe CExpr)
parseCStructDeclr  = 
      parseCDeclr
      `action` (\declr -> (Just declr, Nothing))
  <|> 
      optMaybe parseCDeclr *> (Just $> (ctoken_ CTokColon -*> parseCConstExpr))

-- parse C enumeration declaration (K&R A8.4)
--
parseCEnum :: CParser CEnum
parseCEnum  = 
      (ctoken CTokEnum *> optMaybe cid *> parseCEnumrList
       `actionAttrs`
	 (\((pos, _), _) -> pos) $
	 \((_, ide), enumrs) -> CEnum ide enumrs)
  <|> (ctoken CTokEnum *> cid
       `actionAttrs`
	 (\(pos, _) -> pos) $
	 \(_, ide) -> CEnum (Just ide) [])
  where
    parseCEnumrList = ctoken_ CTokLBrace -*>
		      seplist1 comma_ (parseCEnumr) *->
		      comma_ `opt` () *->
		      ctoken_ CTokRBrace

    parseCEnumr = cid *> optMaybe (ctoken_ CTokAssign -*> parseCConstExpr)

-- parse C declarator (K&R A8.5)
--
-- * removed left-recursion (the parser after the `base' returns a function,
--   which is applied to the result of the prefix to construct the complete
--   tree)
--
-- * the parser for `optPointer' also returns a function; it is applied last,
--   because indirection has the least precedence
--
-- * We allow GNU C attribute annotations as prefix and suffix of declarators, 
--   but they are not entered into the structure tree.
--
parseCDeclr :: CParser CDeclr
parseCDeclr  =
      optMaybe parseGnuCAttr
  -*> (pointer `opt` id)
  *>  base
  *>  many (flip (.)) id (arrayType <|> newStyleFun <|> oldStyleFun)
  *-> optMaybe parseGnuCAttr
  `action`
    \((ptr, base), declrTrans) -> ptr . declrTrans $ base 
  where
    pointer = list1 (ctoken CTokStar *> list parseCTypeQual)
	      `actionAttrs`
		(\qss -> fst . head $ qss) $
		\qss at -> \declr -> CPtrDeclr (map snd qss) declr at

    base =     (cid
		`actionAttrs`
		  posOf $
		  \ide at -> CVarDeclr (Just ide) at)
	   <|> 
	       (    ctoken_ CTokLParen
	        -*> parseCDeclr
	        *-> ctoken_ CTokRParen
		`action`
		  \declr -> declr)

    arrayType =     ctoken CTokLBracket
		*>  optMaybe (parseCConstExpr)
		*-> ctoken_ CTokRBracket
		`actionAttrs`
		  (\(pos, _) -> pos) $
		  \(_, expr) at -> (\declr -> CArrDeclr declr expr at)

    newStyleFun =     ctoken CTokLParen
		  *>  parseCParmTypeList
		  *-> ctoken_ CTokRParen
		`actionAttrs`
		  (\(pos, _) -> pos) $
		  \(_, (parms, variadic)) at -> 
		    (\declr -> CFunDeclr declr parms variadic at)

    oldStyleFun =     ctoken CTokLParen
		  *-> list cid			-- throw away the ide names
		  *-> ctoken_ CTokRParen
		`actionAttrs`
		  id $
		  \_ at -> (\declr -> CFunDeclr declr [] False at)

-- parse GNU C attribute annotation (junking the result)
--
parseGnuCAttr :: CParser ()
parseGnuCAttr  = 
     ctoken_ (CTokGnuC GnuCAttrTok) 
  *> ctoken_ CTokLParen *> ctoken_ CTokLParen
  *> parseAttr
  *> ctoken_ CTokRParen *> ctoken_ CTokRParen
  `action`
    const ()
  where
    parseAttr    =     cid *> optMaybe (   ctoken_ CTokLParen 
					*> seplist comma_ parseAttrArg
					*> ctoken_ CTokRParen)
			`action` const ()
                    <|> ctoken_ CTokConst `action` const ()
    parseAttrArg =      cid  `action` const ()
		    <|> cint `action` const ()
		    <|> cstr `action` const ()

-- parse C parameter type list (K&R A8.6.3)
--
parseCParmTypeList :: CParser ([CDecl], Bool)
parseCParmTypeList  = 
  seplist1 comma_ (parseCParmDecl) *> optBool (comma_ *> ctoken_ CTokEllipsis)
  where
    parseCParmDecl =    
         list1 parseCDeclSpec 
      *> optMaybe (parseCDeclr <|> parseCAbstrDeclr)
      `actionAttrs`
	(\(specs, _) -> (head . map posOf) specs) $
	\(specs, declr) at -> case declr of
			        Nothing -> CDecl specs [] at
		                _       -> 
			          CDecl specs [(declr, Nothing, Nothing)] at

-- parse C initializer (K&R AA8.7)
--
parseCInit :: CParser CInit
parseCInit  =     (parseCAssigExpr 
	           `actionAttrs`
	             (\expr -> posOf expr) $
		     \expr at -> CInitExpr expr at)
	      <|> 
	          (    ctoken CTokLBrace
		   *>  seplist1 comma_ parseCInit
		   *-> ctoken_ CTokRBrace
		   *-> comma_ `opt` ()
		   `actionAttrs`
		     (\(pos, _) -> pos) $
		     \(_, inits) at -> CInitList inits at)

-- parse C type name (K&R A8.8)
--
parseCTypeName :: CParser CDecl
parseCTypeName  =    list1 parseCSpecQual 
		  *> (singleton $> parseCAbstrDeclr) `opt` []
		  `actionAttrs`
		    (\(specs, _) -> (posOf . head) specs) $
		    \(specs, declr) at -> CDecl specs declr at
		  where
		    singleton declr = [(Just declr, Nothing, Nothing)]

-- parse C abstract declarator (K&R A8.8)
--
-- * following K&R, we do not allow old style function types (except empty
--   argument lists) in abstract declarators; unfortunately, gcc allows them
--
parseCAbstrDeclr :: CParser CDeclr
parseCAbstrDeclr  =
      (   pointer 
       *> directAbstrDeclr `opt` emptyDeclr
       `action`
         \(ptr, declr) -> ptr declr)
  <|>
      directAbstrDeclr
  where
    emptyDeclr = CVarDeclr Nothing (newAttrsOnlyPos nopos)

    pointer = list1 (ctoken CTokStar *> list parseCTypeQual)
	      `actionAttrs`
	        (\qss -> fst . head $ qss) $
		\qss at -> \declr -> CPtrDeclr (map snd qss) declr at

    directAbstrDeclr =    (   parentAbstrDeclr
		           *> abstrDeclrSuffix `opt` id
		           `action`
		             \(base, declrTrans) -> declrTrans base)
		       <|>
		          (abstrDeclrSuffix
			   `action`
			     \declrTrans -> declrTrans emptyDeclr)
		       where
		         parentAbstrDeclr = (    ctoken_ CTokLParen
					     -*> parseCAbstrDeclr
					     *-> ctoken_ CTokRParen)
			 abstrDeclrSuffix = many1 (flip (.)) 
					      (arrayType <|> newStyleFun)

    arrayType =     ctoken CTokLBracket
		*>  optMaybe (parseCConstExpr)
		*-> ctoken_ CTokRBracket
		`actionAttrs`
		  (\(pos, _) -> pos) $
		  \(_, expr) at -> (\declr -> CArrDeclr declr expr at)

    newStyleFun =     ctoken CTokLParen
		  *>  (parseCParmTypeList `opt` ([], False))
		  *-> ctoken_ CTokRParen
		`actionAttrs`
		  (\(pos, _) -> pos) $
		  \(_, (parms, variadic)) at -> 
		    (\declr -> CFunDeclr declr parms variadic at)

-- C expression are left-recursive, which requires a left-factorization as we
-- have to get a LL(1) grammar.  The C grammar complicates this by definition
-- of assignments as expressions, which may only have a unary-expression on
-- the left-hand side, but a conditional-expression or even another assignment 
-- on the right-hand side.  As a consequence, the parsers for all expressions
-- "between" unary-expressions and conditional-expressions may only parse
-- whatever is left of their alternatives after removing a unary-expression on 
-- the left.  Therefore, they return a function that given the missing
-- unary-expression yields the actual structure tree for the expression.

-- parse C primary expression (K&R A7.2)
--
-- * contrary to K&R, we regard parsing strings as parsing constants
--
parseCPrimExpr :: CParser CExpr
parseCPrimExpr  =     (cid
		       `actionAttrs`
		         (\ide -> posOf ide) $
			 \ide at -> CVar ide at)
		  <|>
		      (parseCConst	-- includes strings
		       `actionAttrs`
		         (\const -> posOf const) $
			 \const at -> CConst const at)
		  <|>
		          ctoken_ CTokLParen
		      -*> parseCExpr
		      *-> ctoken_ CTokRParen

--parse C postfix expression (K&R A7.3)
--
parseCPostfixExpr :: CParser CExpr
parseCPostfixExpr  = 
  parseCPrimExpr *> many (flip (.)) id suffix
  `action`
    \(prim, suffixFun) -> suffixFun prim
  where
    suffix =     (    ctokenAttrs CTokLBracket
		  *>  parseCExpr 
		  *-> ctoken_ CTokRBracket
		  `action`
		    \(at, expr) -> \e -> CIndex e expr at)
	     <|> 
	         (    ctokenAttrs CTokLParen
		  *>  seplist comma_ parseCAssigExpr
		  *-> ctoken_ CTokRParen
		  `action`
		    \(at, exprs) -> \e -> CCall e exprs at)
	     <|>
	         (ctokenAttrs CTokDot *> cid
		  `action`
		    \(at, ide) -> \e -> CMember e ide False at)
	     <|>
	         (ctokenAttrs CTokArrow *> cid
		  `action`
		    \(at, ide) -> \e -> CMember e ide True at)
	     <|>
	         (ctokenAttrs CTokInc
		  `action`
		    \at -> \e -> CUnary CPostIncOp e at)
	     <|>
	         (ctokenAttrs CTokDec
		  `action`
		    \at -> \e -> CUnary CPostDecOp e at)

-- parse C unary expression (K&R A7.4)
--
parseCUnaryExpr :: CParser CExpr
parseCUnaryExpr  = 
      parseCPostfixExpr
  <|>
      ctokenAttrs CTokInc *> parseCUnaryExpr
      `action`
        (\(at, expr) -> CUnary CPreIncOp expr at)
  <|>
      ctokenAttrs CTokDec *> parseCUnaryExpr
      `action`
        (\(at, expr) -> CUnary CPreDecOp expr at)
  <|>
      parseCUnaryOp *> parseCCastExpr *> parseCUnaryExpr
      `action`
        (\(((op, at), cast), unary) -> CUnary op (cast unary) at)
  <|>
      ctokenAttrs CTokSizeof *> parseCUnaryExpr
      `action`
        (\(at, expr) -> CSizeofExpr expr at)
  <|>
         ctokenAttrs CTokSizeof 
      *-> ctoken_ CTokLParen *> parseCTypeName *-> ctoken_ CTokRParen
      `action`
        (\(at, tname) -> CSizeofType tname at)
  where
    parseCUnaryOp =     ctoken CTokAmper  `opAction` CAdrOp
		    <|> ctoken CTokStar   `opAction` CIndOp
		    <|> ctoken CTokPlus   `opAction` CPlusOp
		    <|> ctoken CTokMinus  `opAction` CMinOp
		    <|> ctoken CTokTilde  `opAction` CCompOp
		    <|> ctoken CTokExclam `opAction` CNegOp

-- parse C cast expression (K&R A7.5)
--
-- * left factorized; we parse here only the prefix before unary-expression,
--   which is pulled up to assignment-expression (as this production is not
--   left-recursive) 
--
parseCCastExpr :: CParser (CExpr -> CExpr)
parseCCastExpr  = many comb id (    ctokenAttrs CTokLParen 
			        *>  parseCTypeName 
				*-> ctoken_ CTokRParen)
  where
    comb (at, typeName) e' = \e -> CCast typeName (e' e) at

-- parse C multiplicative expression (K&R A7.6)
--
-- * left factorized; we recurse here to unary-expression, as cast-expression
--   is not left recursive, and therefore, pulled up to assignment expression, 
--   where the left factorizion starts
--
parseCMulExpr :: CParser (CExpr -> CExpr)
parseCMulExpr  =
  many comb id (seps *> parseCCastExpr *> parseCUnaryExpr)
  where
    comb (((op, at), r'), r) l = \e-> CBinary op (l e) (r' r) at

    seps =     ctoken CTokStar    `opAction` CMulOp
	   <|> ctoken CTokSlash   `opAction` CDivOp
	   <|> ctoken CTokPercent `opAction` CRmdOp

-- parse C additive expression (K&R A7.7)
--
-- * left factorized
--
parseCAddExpr :: CParser (CExpr -> CExpr)
parseCAddExpr  = genParseBinExpr parseCMulExpr [(CTokPlus, CAddOp),
					        (CTokMinus, CSubOp)]

-- parse C shift expression (K&R A7.8)
--
-- * left factorized
--
parseCShiftExpr :: CParser (CExpr -> CExpr)
parseCShiftExpr  = genParseBinExpr parseCAddExpr [(CTokShiftL, CShlOp),
					          (CTokShiftR, CShrOp)]

-- parse C relational expression (K&R A7.9)
--
-- * left factorized
--
parseCRelExpr :: CParser (CExpr -> CExpr)
parseCRelExpr  = genParseBinExpr parseCShiftExpr [(CTokLess, CLeOp),
					          (CTokHigh, CGrOp),
					          (CTokLessEq, CLeqOp),
					          (CTokHighEq, CGeqOp)]

-- parse C equality expression (K&R A7.10)
--
-- * left factorized
--
parseCEqExpr :: CParser (CExpr -> CExpr)
parseCEqExpr  = genParseBinExpr parseCRelExpr [(CTokEqual, CEqOp),
					       (CTokUnequal, CNeqOp)]

-- parse C bitwise and expression (K&R A7.11)
--
-- * left factorized
--
parseCAndExpr :: CParser (CExpr -> CExpr)
parseCAndExpr  = genParseBinExpr parseCEqExpr [(CTokAmper, CAndOp)]

-- parse C bitwise exclusive or expression (K&R A7.12)
--
-- * left factorized
--
parseCXorExpr :: CParser (CExpr -> CExpr)
parseCXorExpr  = genParseBinExpr parseCAndExpr [(CTokHat, CXorOp)]

-- parse C bitwise or expression (K&R A7.13)
--
-- * left factorized
--
parseCOrExpr :: CParser (CExpr -> CExpr)
parseCOrExpr  = genParseBinExpr parseCXorExpr [(CTokBar, COrOp)]

-- parse C logical and expression (K&R A7.14)
--
-- * left factorized
--
parseCLAndExpr :: CParser (CExpr -> CExpr)
parseCLAndExpr  = genParseBinExpr parseCOrExpr [(CTokAnd, CLndOp)]

-- parse C logical or expression (K&R A7.15)
--
-- * left factorized
--
parseCLOrExpr :: CParser (CExpr -> CExpr)
parseCLOrExpr  = genParseBinExpr parseCLAndExpr [(CTokOr, CLorOp)]

-- generic form for the left-factorized binary expression
--
genParseBinExpr :: CParser (CExpr -> CExpr)   -- next level
		-> [(Position -> CToken,      -- separator
		     CBinaryOp)]	      -- generated binary operator
		-> CParser (CExpr -> CExpr)
genParseBinExpr next tokOps =
     next
  *> many comb id (seps *> parseCFullNextExpr)
  `action`
    \(e', cont) -> \e -> cont (e' e)
  where
    comb ((op, at), r) l = \e-> CBinary op (l e) r at

    seps = foldr1 (<|>) [ctoken tok `opAction` op | (tok, op) <- tokOps]

    parseCFullNextExpr = parseCCastExpr *> parseCUnaryExpr *> next
			 `action`
			   \((cast, unary), nextFun) -> cast (nextFun unary)

-- parse C conditional expression (K&R A7.16)
--
-- * left factorized
--
parseCCondExpr :: CParser (CExpr -> CExpr)
parseCCondExpr  = parseCLOrExpr *> condPart `opt` id
		  `action` \(orFun, condFun) -> \expr -> condFun (orFun expr)
		  where
		    condPart =     ctoken CTokQuest *> parseCExpr 
		               *-> ctoken_ CTokColon *> parseCFullCondExpr
			       `actionAttrs`
			         (\((pos, _), _) -> pos) $
				 \((_, te), ee) at -> \ce -> CCond ce te ee at

		    parseCFullCondExpr = 
		      parseCUnaryExpr *> parseCCondExpr
		      `action`
		        \(unaryExpr, condFun)-> condFun unaryExpr

-- parse C assignment expression (K&R A7.17)
--
-- * left factorized
--
parseCAssigExpr :: CParser CExpr
parseCAssigExpr  = 
      parseCCastExpr *> parseCUnaryExpr *> parseCCondExpr
      `action`
        (\((cast, unary), cond ) -> cond (cast unary))
  <|>
      parseCUnaryExpr *> parseCAssigOp *> parseCAssigExpr
      `action`
        (\((lhs, (assOp, at)), rhs) -> CAssign assOp lhs rhs at)
  where
    parseCAssigOp =     ctoken CTokAssign   `opAction` CAssignOp
		    <|> ctoken CTokStarAss  `opAction` CMulAssOp
		    <|> ctoken CTokSlashAss `opAction` CDivAssOp
		    <|> ctoken CTokPercAss  `opAction` CRmdAssOp
		    <|> ctoken CTokPlusAss  `opAction` CAddAssOp
		    <|> ctoken CTokMinusAss `opAction` CSubAssOp
		    <|> ctoken CTokSLAss    `opAction` CShlAssOp
		    <|> ctoken CTokSRAss    `opAction` CShrAssOp
		    <|> ctoken CTokAmpAss   `opAction` CAndAssOp
		    <|> ctoken CTokHatAss   `opAction` CXorAssOp
		    <|> ctoken CTokBarAss   `opAction` COrAssOp

-- parse C expression (K&R A7.18)
--
parseCExpr :: CParser CExpr
parseCExpr  = seplist1 comma_ parseCAssigExpr
	      `actionAttrs`
	        (\exprs -> (posOf . head) exprs) $
		\exprs at -> CComma exprs at

-- parse C constant expression (K&R A7.19)
--
-- * parsing of conditional expression is left-factorized by unary expressions
--
parseCConstExpr :: CParser CExpr
parseCConstExpr  = parseCUnaryExpr *> parseCCondExpr
		   `action`
		     \(unaryExpr, condFun)-> condFun unaryExpr

-- parse C constants
--
-- * we include strings in constants
--
parseCConst :: CParser CConst
parseCConst  =     (cint   `actionAttrs` snd $ CIntConst . fst)
	       <|> (cchar  `actionAttrs` snd $ CCharConst . fst)
	       <|> (cfloat `actionAttrs` snd $ CFloatConst . fst)
	       <|> (cstr   `actionAttrs` snd $ CStrConst . fst)


-- main parsing routine
-- --------------------

-- parse the cpp-preprocessed C header contained contained in the given string 
-- (EXPORTED)
--
-- * the given position is attributed to the first character in the string
--
-- * errors are entered into the compiler state
--
parseC         :: String -> Position -> CST s CHeader
parseC inp pos  = do
		    tokens <- lexC inp pos
		    parseCHeader pos tokens
