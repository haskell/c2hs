--  C -> Haskell Compiler: Abstract Syntax for Header Files
--
--  Author : Manuel M T Chakravarty
--  Created: 7 March 99
--
--  Version $Revision: 1.10 $ from $Date: 2004/06/11 07:10:16 $
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
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
--  Abstract syntax of C header files.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The tree structure corresponds to the grammar in Appendix A of K&R.  This
--  abstract syntax simplifies the concrete syntax by merging similar concrete
--  constructs into a single type of abstract tree structure: declarations are
--  merged with structure declarations, parameter declarations and type names,
--  and declarators are merged with abstract declarators.
--
--  With K&R we refer to ``The C Programming Language'', second edition, Brain
--  W. Kernighan and Dennis M. Ritchie, Prentice Hall, 1988.  This module
--  supports the C99 `restrict' extension
--  <http://www.lysator.liu.se/c/restrict.html>, `inline' functions, and also
--  the GNU C `alignof' extension.
--
--- TODO ----------------------------------------------------------------------
--

module CAST (CHeader(..), CExtDecl(..), CFunDef(..), CStat(..), CDecl(..),
	     CDeclSpec(..), CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
	     CStructUnion(..),  CStructTag(..), CEnum(..), CDeclr(..),
	     CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..), CUnaryOp(..),
	     CConst (..)) 
where

import Common     (Position, Pos(posOf), nopos)
import Idents     (Ident)
import Attributes (Attrs)


-- a complete C header file (K&R A10) (EXPORTED)
--
data CHeader = CHeader [CExtDecl]
		       Attrs

instance Pos CHeader where
  posOf (CHeader _ at) = posOf at

instance Eq CHeader where
  (CHeader _ at1) == (CHeader _ at2) = at1 == at2

-- external C declaration (K&R A10) (EXPORTED)
--
data CExtDecl = CDeclExt CDecl
	      | CFDefExt CFunDef
	      | CAsmExt  Attrs		-- a chunk of assembly code (which is
					-- not itself recorded)

instance Pos CExtDecl where
  posOf (CDeclExt decl) = posOf decl
  posOf (CFDefExt fdef) = posOf fdef
  posOf (CAsmExt at)	= posOf at

instance Eq CExtDecl where
  CDeclExt decl1 == CDeclExt decl2 = decl1 == decl2
  CFDefExt fdef1 == CFDefExt fdef2 = fdef1 == fdef2
  CAsmExt at1    == CAsmExt at2    =   at1 == at2

-- C function definition (K&R A10.1) (EXPORTED)
--
-- * The only type specifiers allowed are `extern' and `static'.
--
-- * The declarator must specify explicitly that the declared identifier has
--   function type.
--
-- * The optional declaration list is for old-style function declarations.
--
-- * The statement must be a compound statement.
--
data CFunDef = CFunDef [CDeclSpec]	-- type specifier and qualifier
		       CDeclr		-- declarator
		       [CDecl]		-- optional declaration list
		       CStat		-- compound statement
		       Attrs

instance Pos CFunDef where
  posOf (CFunDef _ _ _ _ at) = posOf at

instance Eq CFunDef where
  CFunDef _ _ _ _ at1 == CFunDef _ _ _ _ at2 = at1 == at2

-- C statement (A9) (EXPORTED)
--
data CStat = CLabel    Ident		-- label
		       CStat
		       Attrs
           | CCase     CExpr		-- constant expression
		       CStat
		       Attrs
           | CDefault  CStat		-- default case
		       Attrs
           | CExpr     (Maybe CExpr)	-- expression statement, maybe empty
		       Attrs
           | CCompound [CDecl]		-- optional declaration list
		       [CStat]		-- optional statement list
		       Attrs
           | CIf       CExpr		-- conditional expression
		       CStat
		       (Maybe CStat)    -- optional "else" case
		       Attrs
           | CSwitch   CExpr	        -- selector
		       CStat
		       Attrs
           | CWhile    CExpr
		       CStat
		       Bool		-- `True' implies "do-while" statement
		       Attrs
           | CFor      (Maybe CExpr)
		       (Maybe CExpr)
		       (Maybe CExpr)
		       CStat
		       Attrs
           | CGoto     Ident		-- label
		       Attrs
           | CCont     Attrs		-- continue statement
	   | CBreak    Attrs		-- break statement
	   | CReturn   (Maybe CExpr)
		       Attrs
	   | CAsm      Attrs		-- a chunk of assembly code (which is
	   				-- not itself recorded)

instance Pos CStat where
  posOf (CLabel    _ _     at) = posOf at
  posOf (CCase     _ _     at) = posOf at
  posOf (CDefault  _       at) = posOf at
  posOf (CExpr     _       at) = posOf at
  posOf (CCompound _ _     at) = posOf at
  posOf (CIf       _ _ _   at) = posOf at
  posOf (CSwitch   _ _     at) = posOf at
  posOf (CWhile    _ _ _   at) = posOf at
  posOf (CFor      _ _ _ _ at) = posOf at
  posOf (CGoto     _	   at) = posOf at
  posOf (CCont     	   at) = posOf at
  posOf (CBreak    	   at) = posOf at
  posOf (CReturn   _   	   at) = posOf at
  posOf (CAsm              at) = posOf at

instance Eq CStat where
  (CLabel    _ _     at1) == (CLabel    _ _     at2) = at1 == at2
  (CCase     _ _     at1) == (CCase     _ _     at2) = at1 == at2
  (CDefault  _       at1) == (CDefault  _       at2) = at1 == at2
  (CExpr     _       at1) == (CExpr     _       at2) = at1 == at2
  (CCompound _ _     at1) == (CCompound _ _     at2) = at1 == at2
  (CIf       _ _ _   at1) == (CIf       _ _ _   at2) = at1 == at2
  (CSwitch   _ _     at1) == (CSwitch   _ _     at2) = at1 == at2
  (CWhile    _ _ _   at1) == (CWhile    _ _ _   at2) = at1 == at2
  (CFor      _ _ _ _ at1) == (CFor      _ _ _ _ at2) = at1 == at2
  (CGoto     _	     at1) == (CGoto     _	at2) = at1 == at2
  (CCont	     at1) == (CCont		at2) = at1 == at2
  (CBreak	     at1) == (CBreak		at2) = at1 == at2
  (CReturn   _	     at1) == (CReturn   _	at2) = at1 == at2
  (CAsm              at1) == (CAsm              at2) = at1 == at2

-- C declaration (K&R A8), structure declaration (K&R A8.3), parameter
-- declaration (K&R A8.6.3), and type name (K&R A8.8) (EXPORTED) 
--
-- * Toplevel declarations (K&R A8): 
--
--   - they require that the type specifier and qualifier list is not empty,
--     but gcc allows it and just issues a warning; for the time being, we
--     also allow it;
--   - at most one storage class specifier is allowed per declaration;
--   - declarators must be present and size expressions are not allowed, ie,
--     the elements of K&R's init-declarator-list are represented by triples
--     of the form `(Just declr, oinit, Nothing)', where `oinit' maybe
--     `Nothing' or `Just init'; and
--   - abstract declarators are not allowed.
--
-- * Structure declarations (K&R A8.3):
--
--   - do not allow storage specifiers;
--   - do not allow initializers; 
--   - require a non-empty declarator-triple list, where abstract declarators 
--     are not allowed; and
--   - each of the declarator-triples has to contain either a declarator or a
--     size expression, or both, ie, it has the form `(Just decl, Nothing,
--     Nothing)', `(Nothing, Nothing, Just size)', or `(Just decl, Nothing,
--     Just size)'.
--
-- * Parameter declarations (K&R A8.6.3):
--
--   - allow neither initializers nor size expressions;
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty); and
--   - allow abstract declarators.
--
-- * Type names (A8.8):
--
--   - do not allow storage specifiers;
--   - allow neither initializers nor size expressions; and
--   - allow at most one declarator triple of the form `(Just declr, Nothing, 
--     Nothing)' (in case of an empty declarator, the list must be empty),
--     where the declarator must be abstract, ie, must not contain a declared
--     identifier. 
--
data CDecl = CDecl [CDeclSpec]		-- type specifier and qualifier
		   [(Maybe CDeclr,	-- declarator (may be omitted)
		     Maybe CInit,	-- optional initializer
		     Maybe CExpr)]	-- optional size (const expr)
		   Attrs

instance Pos CDecl where
  posOf (CDecl _ _ at) = posOf at

instance Eq CDecl where
  (CDecl _ _ at1) == (CDecl _ _ at2) = at1 == at2

-- C declaration specifiers and qualifiers (EXPORTED)
--
data CDeclSpec = CStorageSpec CStorageSpec
	       | CTypeSpec    CTypeSpec
	       | CTypeQual    CTypeQual
	       deriving (Eq)

instance Pos CDeclSpec where
  posOf (CStorageSpec sspec) = posOf sspec
  posOf (CTypeSpec    tspec) = posOf tspec
  posOf (CTypeQual    tqual) = posOf tqual

-- C storage class specifier (K&R A8.1) (EXPORTED)
--
data CStorageSpec = CAuto     Attrs
		  | CRegister Attrs
		  | CStatic   Attrs
		  | CExtern   Attrs
		  | CTypedef  Attrs	-- syntactic awkwardness of C

instance Pos CStorageSpec where
  posOf (CAuto     at) = posOf at
  posOf (CRegister at) = posOf at
  posOf (CStatic   at) = posOf at
  posOf (CExtern   at) = posOf at
  posOf (CTypedef  at) = posOf at

instance Eq CStorageSpec where
  (CAuto     at1) == (CAuto     at2) = at1 == at2
  (CRegister at1) == (CRegister at2) = at1 == at2
  (CStatic   at1) == (CStatic   at2) = at1 == at2
  (CExtern   at1) == (CExtern   at2) = at1 == at2
  (CTypedef  at1) == (CTypedef  at2) = at1 == at2

-- C type specifier (K&R A8.2) (EXPORTED)
--
data CTypeSpec = CVoidType    Attrs
	       | CCharType    Attrs
	       | CShortType   Attrs
	       | CIntType     Attrs
	       | CLongType    Attrs
	       | CFloatType   Attrs
	       | CDoubleType  Attrs
	       | CSignedType  Attrs
	       | CUnsigType   Attrs
	       | CSUType      CStructUnion
			      Attrs
	       | CEnumType    CEnum
			      Attrs
	       | CTypeDef     Ident		-- typedef name
			      Attrs

instance Pos CTypeSpec where
  posOf (CVoidType      at) = posOf at
  posOf (CCharType      at) = posOf at
  posOf (CShortType     at) = posOf at
  posOf (CIntType       at) = posOf at
  posOf (CLongType      at) = posOf at
  posOf (CFloatType     at) = posOf at
  posOf (CDoubleType    at) = posOf at
  posOf (CSignedType    at) = posOf at
  posOf (CUnsigType     at) = posOf at
  posOf (CSUType     _  at) = posOf at
  posOf (CEnumType   _  at) = posOf at
  posOf (CTypeDef    _  at) = posOf at

instance Eq CTypeSpec where
  (CVoidType     at1) == (CVoidType     at2) = at1 == at2
  (CCharType     at1) == (CCharType     at2) = at1 == at2
  (CShortType    at1) == (CShortType    at2) = at1 == at2
  (CIntType      at1) == (CIntType      at2) = at1 == at2
  (CLongType     at1) == (CLongType     at2) = at1 == at2
  (CFloatType    at1) == (CFloatType    at2) = at1 == at2
  (CDoubleType   at1) == (CDoubleType   at2) = at1 == at2
  (CSignedType   at1) == (CSignedType   at2) = at1 == at2
  (CUnsigType    at1) == (CUnsigType    at2) = at1 == at2
  (CSUType     _ at1) == (CSUType     _ at2) = at1 == at2
  (CEnumType   _ at1) == (CEnumType   _ at2) = at1 == at2
  (CTypeDef    _ at1) == (CTypeDef    _ at2) = at1 == at2

-- C type qualifier (K&R A8.2) (EXPORTED)
--
-- * plus `restrict' from C99 and `inline'
--
data CTypeQual = CConstQual Attrs
	       | CVolatQual Attrs
	       | CRestrQual Attrs
	       | CInlinQual Attrs

instance Pos CTypeQual where
 posOf (CConstQual at) = posOf at
 posOf (CVolatQual at) = posOf at
 posOf (CRestrQual at) = posOf at
 posOf (CInlinQual at) = posOf at

instance Eq CTypeQual where
  (CConstQual at1) == (CConstQual at2) = at1 == at2
  (CVolatQual at1) == (CVolatQual at2) = at1 == at2
  (CRestrQual at1) == (CRestrQual at2) = at1 == at2
  (CInlinQual at1) == (CInlinQual at2) = at1 == at2

-- C structure of union declaration (K&R A8.3) (EXPORTED)
--
-- * in both case, either the identifier is present or the list must be
--   non-empty 
--
data CStructUnion = CStruct CStructTag
			    (Maybe Ident)
			    [CDecl]	-- *structure* declaration
			    Attrs

instance Pos CStructUnion where
  posOf (CStruct _ _ _ at) = posOf at

instance Eq CStructUnion where
  (CStruct _ _ _ at1) == (CStruct _ _ _ at2) = at1 == at2

-- (EXPORTED)
--
data CStructTag = CStructTag
		| CUnionTag
		deriving (Eq)

-- C enumeration declaration (K&R A8.4) (EXPORTED)
--
data CEnum = CEnum (Maybe Ident)
		   [(Ident,			-- variant name
		     Maybe CExpr)]		-- explicit variant value
		   Attrs

instance Pos CEnum where
  posOf (CEnum _ _ at) = posOf at

instance Eq CEnum where
  (CEnum _ _ at1) == (CEnum _ _ at2) = at1 == at2

-- C declarator (K&R A8.5) and abstract declarator (K&R A8.8) (EXPORTED)
--
-- * We have one type qualifer list `[CTypeQual]' for each indirection (ie,
--   each occurrence of `*' in the concrete syntax).
--
-- * We unfold K&R's direct-declarators nonterminal into declarators.  Note
--   that `*(*x)' is equivalent to `**x'.
--
-- * Declarators (A8.5) and abstract declarators (A8.8) are represented in the 
--   same structure.  In the case of a declarator, the identifier in
--   `CVarDeclr' must be present; in an abstract declarator it misses.
--   `CVarDeclr Nothing ...' on its own is meaningless, it may only occur as
--   part of a larger type (ie, there must be a pointer, an array, or function
--   declarator around).
--
-- * The qualifiers list in a `CPtrDeclr' may not be empty.
--
-- * Old and new style function definitions are merged into a single case
--   `CFunDeclr'.  In case of an old style definition, the parameter list is
--   empty and the variadic flag is `False' (ie, the parameter names are not
--   stored in the tree).  Remember, a new style definition with no parameters 
--   requires a single `void' in the argument list (according to the standard).
--
-- * We unfold K&R's parameter-type-list nonterminal into the declarator
--   variant for functions.
--
data CDeclr = CVarDeclr (Maybe Ident)		-- declared identifier
		        Attrs
	    | CPtrDeclr [[CTypeQual]]		-- indirections (non-empty)
		        CDeclr
		        Attrs
            | CArrDeclr CDeclr
			(Maybe CExpr)		-- array size
			Attrs
	    | CFunDeclr CDeclr
			[CDecl]			-- *parameter* declarations
			Bool			-- is variadic?
			Attrs

instance Pos CDeclr where
  posOf (CVarDeclr _     at) = posOf at
  posOf (CPtrDeclr _ _   at) = posOf at
  posOf (CArrDeclr _ _   at) = posOf at
  posOf (CFunDeclr _ _ _ at) = posOf at

instance Eq CDeclr where
  (CVarDeclr _     at1) == (CVarDeclr _     at2) = at1 == at2
  (CPtrDeclr _ _   at1) == (CPtrDeclr _ _   at2) = at1 == at2
  (CArrDeclr _ _   at1) == (CArrDeclr _ _   at2) = at1 == at2
  (CFunDeclr _ _ _ at1) == (CFunDeclr _ _ _ at2) = at1 == at2

-- C initializer (K&R A8.7) (EXPORTED)
--
data CInit = CInitExpr CExpr
		       Attrs		-- assignment expression
           | CInitList [CInit]
		       Attrs

instance Pos CInit where
  posOf (CInitExpr _ at) = posOf at
  posOf (CInitList _ at) = posOf at

instance Eq CInit where
  (CInitExpr _ at1) == (CInitExpr _ at2) = at1 == at2
  (CInitList _ at1) == (CInitList _ at2) = at1 == at2

-- C expression (K&R A7) (EXPORTED)
--
-- * these can be arbitrary expression, as the argument of `sizeof' can be
--   arbitrary, even if appearing in a constant expression
--
-- * GNU C extension: `alignof'
--
data CExpr = CComma       [CExpr]	-- comma expression list, n >= 2
		          Attrs
	   | CAssign      CAssignOp	-- assignment operator
		          CExpr		-- l-value
		          CExpr		-- r-value
		          Attrs
	   | CCond        CExpr		-- conditional
		          CExpr		-- true-expression
		          CExpr		-- false-expression
		          Attrs
	   | CBinary      CBinaryOp	-- binary operator
		          CExpr		-- lhs
		          CExpr		-- rhs
		          Attrs
	   | CCast        CDecl		-- type name
		          CExpr
		          Attrs
           | CUnary       CUnaryOp	-- unary operator
		          CExpr
		          Attrs
	   | CSizeofExpr  CExpr
			  Attrs
	   | CSizeofType  CDecl		-- type name
			  Attrs
	   | CAlignofExpr CExpr
			  Attrs
	   | CAlignofType CDecl		-- type name
			  Attrs
	   | CIndex       CExpr		-- array
			  CExpr		-- index
			  Attrs
	   | CCall	  CExpr		-- function
			  [CExpr]	-- arguments
			  Attrs
	   | CMember	  CExpr		-- structure
			  Ident		-- member name
			  Bool		-- deref structure? (True for `->')
			  Attrs
	   | CVar	  Ident		-- identifier (incl. enumeration const)
			  Attrs
           | CConst       CConst		-- includes strings
			  Attrs

instance Pos CExpr where
  posOf (CComma       _     at) = posOf at
  posOf (CAssign      _ _ _ at) = posOf at
  posOf (CCond        _ _ _ at) = posOf at
  posOf (CBinary      _ _ _ at) = posOf at
  posOf (CCast        _ _   at) = posOf at
  posOf (CUnary       _ _   at) = posOf at
  posOf (CSizeofExpr  _     at) = posOf at
  posOf (CSizeofType  _     at) = posOf at
  posOf (CAlignofExpr _     at) = posOf at
  posOf (CAlignofType _     at) = posOf at
  posOf (CIndex       _ _   at) = posOf at
  posOf (CCall        _ _   at) = posOf at
  posOf (CMember      _ _ _ at) = posOf at
  posOf (CVar         _     at) = posOf at
  posOf (CConst       _     at) = posOf at

instance Eq CExpr where
  (CComma      	_     at1) == (CComma       _     at2) = at1 == at2
  (CAssign     	_ _ _ at1) == (CAssign      _ _ _ at2) = at1 == at2
  (CCond       	_ _ _ at1) == (CCond        _ _ _ at2) = at1 == at2
  (CBinary     	_ _ _ at1) == (CBinary      _ _ _ at2) = at1 == at2
  (CCast       	_ _   at1) == (CCast        _ _   at2) = at1 == at2
  (CUnary      	_ _   at1) == (CUnary       _ _   at2) = at1 == at2
  (CSizeofExpr 	_     at1) == (CSizeofExpr  _     at2) = at1 == at2
  (CSizeofType 	_     at1) == (CSizeofType  _     at2) = at1 == at2
  (CAlignofExpr _     at1) == (CAlignofExpr _     at2) = at1 == at2
  (CAlignofType _     at1) == (CAlignofType _     at2) = at1 == at2
  (CIndex      	_ _   at1) == (CIndex       _ _   at2) = at1 == at2
  (CCall       	_ _   at1) == (CCall	    _ _   at2) = at1 == at2
  (CMember     	_ _ _ at1) == (CMember	    _ _ _ at2) = at1 == at2
  (CVar        	_     at1) == (CVar	    _     at2) = at1 == at2
  (CConst      	_     at1) == (CConst	    _	  at2) = at1 == at2

-- C assignment operators (K&R A7.17) (EXPORTED)
--
data CAssignOp = CAssignOp
	       | CMulAssOp
	       | CDivAssOp
	       | CRmdAssOp		-- remainder and assignment
	       | CAddAssOp
	       | CSubAssOp
	       | CShlAssOp
	       | CShrAssOp
	       | CAndAssOp
	       | CXorAssOp
	       | COrAssOp
	       deriving (Eq)

-- C binary operators (K&R A7.6-15) (EXPORTED)
--
data CBinaryOp = CMulOp
	       | CDivOp
	       | CRmdOp			-- remainder of division
	       | CAddOp
	       | CSubOp
	       | CShlOp			-- shift left
	       | CShrOp			-- shift right
	       | CLeOp			-- less
	       | CGrOp			-- greater
	       | CLeqOp			-- less or equal
	       | CGeqOp			-- greater or equal
	       | CEqOp			-- equal
	       | CNeqOp			-- not equal
	       | CAndOp			-- bitwise and
	       | CXorOp			-- exclusive bitwise or
	       | COrOp			-- inclusive bitwise or
	       | CLndOp			-- logical and
	       | CLorOp			-- logical or
	       deriving (Eq)

-- C unary operator (K&R A7.3-4) (EXPORTED)
--
data CUnaryOp = CPreIncOp		-- prefix increment operator
	      | CPreDecOp		-- prefix decrement operator
	      | CPostIncOp		-- postfix increment operator
	      | CPostDecOp		-- postfix decrement operator
	      | CAdrOp			-- address operator
	      | CIndOp			-- indirection operator
	      | CPlusOp			-- prefix plus
	      | CMinOp			-- prefix minus
	      | CCompOp			-- one's complement
	      | CNegOp			-- logical negation
	      deriving (Eq)

-- C constant (K&R A2.5 & A7.2) (EXPORTED)
--
-- * we do not list enumeration constants here, as they are identifiers
--
data CConst = CIntConst   Integer
		          Attrs
	    | CCharConst  Char
		          Attrs
	    | CFloatConst String
			  Attrs
	    | CStrConst   String
			  Attrs

instance Pos CConst where
  posOf (CIntConst   _ at) = posOf at
  posOf (CCharConst  _ at) = posOf at
  posOf (CFloatConst _ at) = posOf at
  posOf (CStrConst   _ at) = posOf at

instance Eq CConst where
  (CIntConst   _ at1) == (CIntConst   _ at2) = at1 == at2
  (CCharConst  _ at1) == (CCharConst  _ at2) = at1 == at2
  (CFloatConst _ at1) == (CFloatConst _ at2) = at1 == at2
  (CStrConst   _ at1) == (CStrConst   _ at2) = at1 == at2
