--  C->Haskell Compiler: pretty printing of C abstract syntax
--
--  Author : Manuel M T Chakravarty
--  Created: 25 August 1
--
--  Version $Revision: 1.2 $ from $Date: 2004/06/11 07:10:16 $
--
--  Copyright (c) [2001..2004] Manuel M T Chakravarty
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
--  Pretty printing support for abstract C trees.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--- TODO ----------------------------------------------------------------------
--
--  * So far, only covers a small fraction of the abstract tree definition
--

module CPretty (
  -- we are just providing instances to the class `Pretty'
) where

import Common (PrettyPrintMode, dftOutWidth, dftOutRibbon)
import Idents (Ident, identToLexeme)
import Pretty (Doc, Pretty(..), empty, text, (<>), (<+>), hsep, hang,
	       punctuate, comma, semi, parens, brackets, render)

import CAST


-- pretty printing of AST nodes
-- ----------------------------

instance Show CDecl where
  showsPrec _ = showString . render . pretty


-- actual structure tree traversals
-- --------------------------------

instance Pretty CDecl where
  pretty (CDecl specs declrs _) =
    hsep (map pretty specs) `hang` 2 $
      hsep (punctuate comma (map prettyDeclr declrs)) <> semi

instance Pretty CDeclSpec where
  pretty (CStorageSpec sspec) = pretty sspec
  pretty (CTypeSpec    tspec) = pretty tspec
  pretty (CTypeQual    qspec) = pretty qspec

instance Pretty CStorageSpec where
  pretty (CAuto     _) = text "auto"
  pretty (CRegister _) = text "register"
  pretty (CStatic   _) = text "static"
  pretty (CExtern   _) = text "extern"
  pretty (CTypedef  _) = text "typedef"

instance Pretty CTypeSpec where
  pretty (CVoidType      _) = text "void"
  pretty (CCharType      _) = text "char"
  pretty (CShortType     _) = text "short"
  pretty (CIntType       _) = text "int"
  pretty (CLongType      _) = text "long"
  pretty (CFloatType     _) = text "float"
  pretty (CDoubleType    _) = text "double"
  pretty (CSignedType    _) = text "signed"
  pretty (CUnsigType     _) = text "unsigned"
  pretty (CSUType struct _) = text "<<CPretty: CSUType not yet implemented!>>"
  pretty (CEnumType enum _) = text "<<CPretty: CEnumType not yet implemented!>>"
  pretty (CTypeDef ide   _) = ident ide

instance Pretty CTypeQual where
  pretty (CConstQual _) = text "const"
  pretty (CVolatQual _) = text "volatile"
  pretty (CRestrQual _) = text "restrict"

prettyDeclr :: (Maybe CDeclr, Maybe CInit, Maybe CExpr) -> Doc
prettyDeclr (odeclr, oinit, oexpr) =
      maybe empty pretty odeclr
  <+> maybe empty ((text "=" <+>) . pretty) oinit
  <+> maybe empty ((text ":" <+>) . pretty) oexpr

instance Pretty CDeclr where
  pretty (CVarDeclr oide                   _) = maybe empty ident oide
  pretty (CPtrDeclr inds declr             _) = 
    let
      oneLevel ind = parens . (hsep (map pretty ind) <+>) . (text "*" <>)
    in
    foldr oneLevel (pretty declr) inds
  pretty (CArrDeclr declr oexpr            _) =
    pretty declr <> brackets (maybe empty pretty oexpr)
  pretty (CFunDeclr declr decls isVariadic _) =
    let
      varDoc = if isVariadic then text ", ..." else empty
    in
    pretty declr 
    <+> parens (hsep (punctuate comma (map pretty decls)) <> varDoc)

instance Pretty CInit where
  pretty _ = text "<<CPretty: CInit not yet implemented!>>"

instance Pretty CExpr where
  pretty _ = text "<<CPretty: CExpr not yet implemented!>>"


-- auxilliary functions
-- --------------------

ident :: Ident -> Doc
ident  = text . identToLexeme
