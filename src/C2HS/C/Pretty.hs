--  C->Haskell Compiler: pretty printing of C abstract syntax
--
--  Author: Bertram Felgenhauer <int-e@gmx.de>
--  Created: 2007-11-10
--
--  Copyright (c) 2007 Bertram Felgenhauer
--  the interface is based on code by Manuel M T Chakravarty
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
module C2HS.C.Pretty (
    Pretty (..)
) where

import C2HS.C.AST
import Data.Idents
import Text.PrettyPrint.HughesPJ

-- provide a Show instance for CDecl for backward compatibility
instance Show CDecl where
    showsPrec _ = showString . render . pretty

-- Pretty class
class Pretty p where
    pretty     :: p -> Doc
    prettyPrec :: Int -> p -> Doc

    pretty       = prettyPrec 0
    prettyPrec _ = pretty

-- pretty print optional chunk
maybeP :: (p -> Doc) -> Maybe p -> Doc
maybeP = maybe empty

-- pretty print identifier
identP :: Ident -> Doc
identP = text . identToLexeme

-- analogous to showParen
parenPrec :: Int -> Int -> Doc -> Doc
parenPrec prec prec2 t = if prec <= prec2 then t else parens t

-- indent a chunk of code
ii :: Doc -> Doc
ii = nest 4

-- Pretty instances
instance Pretty CHeader where
    pretty (CHeader edecls _) = vcat (map pretty edecls)

instance Pretty CExtDecl where
    pretty (CDeclExt decl) = pretty decl <> semi
    pretty (CFDefExt fund) = pretty fund
    pretty (CAsmExt  _   ) = text "[[[asm]]];"

instance Pretty CFunDef where
    pretty (CFunDef declspecs declr decls stat _) =
        hsep (map pretty declspecs) <+> pretty declr $$ prettyPrec (-1) stat

instance Pretty CStat where
    pretty (CLabel ident stat _) = identP ident <> text ":" $$ pretty stat
    pretty (CCase expr stat _) =
        text "case" <+> pretty expr <> text ":" $$ pretty stat
    pretty (CCases expr1 expr2 stat _) =
        text "case" <+> pretty expr1 <> text ".."
                    <>  pretty expr2 <> text ":" $$ pretty stat
    pretty (CDefault stat _) = text "default:" $$ pretty stat
    pretty (CExpr expr _) = ii $ maybeP pretty expr <> semi
    pretty c@(CCompound _ _) = prettyPrec 0 c
    pretty (CIf expr stat estat _) =
        ii $ text "if" <+> text "(" <> pretty expr <> text ")"
               $+$ prettyPrec (-1) stat
               $$ maybeP ((text "else" $+$) . prettyPrec (-1)) estat
    pretty (CSwitch expr stat _) =
        ii $ text "switch" <+> text "(" <> pretty expr <> text ")"
               $+$ prettyPrec (-1) stat
    pretty (CWhile expr stat False _) =
        ii $ text "while" <+> text "(" <> pretty expr <> text ")"
               $+$ prettyPrec (-1) stat
    pretty (CWhile expr stat True _) =
        ii $ text "do" $+$ prettyPrec (-1) stat
               $$ text "while" <+> text "(" <> pretty expr <> text ");"
    pretty (CFor init cond step stat _) =
        ii $ text "for" <+> text "("
               <> either (maybeP pretty) pretty init <> semi
               <+> maybeP pretty cond <> semi
               <+> maybeP pretty step <> text ")" $+$ prettyPrec (-1) stat
    pretty (CGoto ident _) = ii $ text "goto" <+> identP ident <> semi
    pretty (CGotoPtr expr _) = ii $ text "goto" <+> pretty expr <> semi
    pretty (CCont _) = ii $ text "continue" <> semi
    pretty (CBreak _) = ii $ text "break" <> semi
    pretty (CReturn Nothing _) = ii $ text "return" <> semi
    pretty (CReturn (Just e) _) = ii $ text "return" <+> pretty e <> semi
    pretty (CAsm _) = ii $ text "[[[asm]]]" <> semi

    prettyPrec p (CCompound bis _) =
        let inner = text "{" $+$ vcat (map pretty bis) $$ text "}"
        in  if p == -1 then inner else ii inner
    prettyPrec _ p = pretty p

instance Pretty CBlockItem where
    pretty (CBlockStmt stat) = pretty stat
    pretty (CBlockDecl decl) = ii $ pretty decl <> semi
    pretty (CNestedFunDef fundef) = ii $ pretty fundef

instance Pretty CDecl where
    pretty (CDecl specs divs _) =
        hsep (map pretty specs) <+> hsep (punctuate comma (map p divs)) where
        p (declr, init, expr) =
            maybeP pretty declr <+>
            maybeP ((text "=" <+>) . pretty) init <+>
            maybeP ((text ":" <+>) . pretty) expr

instance Pretty CDeclSpec where
    pretty (CStorageSpec sp) = pretty sp
    pretty (CTypeSpec sp) = pretty sp
    pretty (CTypeQual qu) = pretty qu

instance Pretty CStorageSpec where
    pretty (CAuto _)     = text "auto"
    pretty (CRegister _) = text "register"
    pretty (CStatic _)   = text "static"
    pretty (CExtern _)   = text "extern"
    pretty (CTypedef _)  = text "typedef"
    pretty (CThread _)   = text "thread"

instance Pretty CTypeSpec where
    pretty (CVoidType _)        = text "void"
    pretty (CCharType _)        = text "char"
    pretty (CShortType _)       = text "short"
    pretty (CIntType _)         = text "int"
    pretty (CLongType _)        = text "long"
    pretty (CFloatType _)       = text "float"
    pretty (CDoubleType _)      = text "double"
    pretty (CSignedType _)      = text "signed"
    pretty (CUnsigType _)       = text "unsigned"
    pretty (CBoolType _)        = text "bool"
    pretty (CComplexType _)     = text "complex"
    pretty (CSUType union _)    = pretty union
    pretty (CEnumType enum _)   = pretty enum
    pretty (CTypeDef ident _)   = identP ident
    pretty (CTypeOfExpr expr _) =
        text "typeof" <> text "(" <> pretty expr <> text ")"
    pretty (CTypeOfType decl _) =
        text "typeof" <> text "(" <> pretty decl <> text ")"

instance Pretty CTypeQual where
    pretty (CConstQual _) = text "const"
    pretty (CVolatQual _) = text "volatile"
    pretty (CRestrQual _) = text "__restrict"
    pretty (CInlinQual _) = text "inline"

instance Pretty CStructUnion where
    pretty (CStruct tag ident [] _) = pretty tag <+> maybeP identP ident
    pretty (CStruct tag ident decls _) = vcat [
        pretty tag <+> maybeP identP ident <+> text "{",
        ii $ sep (map (<> semi) (map pretty decls)),
        text "}"]

instance Pretty CStructTag where
    pretty CStructTag = text "struct"
    pretty CUnionTag  = text "union"

instance Pretty CEnum where
    pretty (CEnum ident vals _) = vcat [
        text "enum" <+> maybeP identP ident <+> text "{",
        ii $ sep (punctuate comma (map p vals)),
        text "}"] where
        p (ident, expr) = identP ident <+> maybeP ((text "=" <+>) . pretty) expr

instance Pretty CDeclr where
    prettyPrec p (CVarDeclr ident _) = maybeP identP ident
    prettyPrec p (CPtrDeclr quals declr _) =
        parenPrec p 5 $ text "*" <> hsep (map pretty quals)
                      <> prettyPrec 5 declr
    prettyPrec p (CArrDeclr declr quals expr _) =
        parenPrec p 5 $ hsep (map pretty quals) <> prettyPrec 6 declr
                      <> text "[" <> maybeP pretty expr <> text "]"
    prettyPrec p (CFunDeclr declr decls var _) =
        prettyPrec 6 declr <> text "("
            <> sep (punctuate comma (map pretty decls))
            <> (if var then text "," <+> text "..." else empty) <> text ")"

instance Pretty CInit where
    pretty (CInitExpr expr _) = pretty expr
    pretty (CInitList initl _) =
        text "{" <+> hsep (punctuate comma (map p initl)) <+> text "}" where
        p (desigs, init) = hsep (map pretty desigs) <> pretty init

instance Pretty CDesignator where
    pretty (CArrDesig expr _) = text "[" <> pretty expr <> text "]"
    pretty (CMemberDesig ident _) = text "." <> identP ident
    pretty (CRangeDesig expr1 expr2 _) =
        text "[" <> pretty expr1 <> text ".." <> pretty expr2 <> text "]"

instance Pretty CExpr where
    prettyPrec p (CComma exprs _) =
        parenPrec p 1 $ hsep (punctuate comma (map (prettyPrec 2) exprs))
    prettyPrec p (CAssign op expr1 expr2 _) =
        parenPrec p 2 $ prettyPrec 3 expr1 <+> pretty op <+> prettyPrec 2 expr2
    prettyPrec p (CCond expr1 expr2 expr3 _) =
        parenPrec p 3 $ prettyPrec 4 expr1 <+> text "?"
           <+> maybeP pretty expr2 <+> text ":" <+> prettyPrec 4 expr3
    prettyPrec p (CBinary op expr1 expr2 _) =
        let prec = binPrec op
        in  parenPrec p prec $ prettyPrec prec expr1
                             <+> pretty op <+> prettyPrec (prec + 1) expr2
    prettyPrec p (CCast decl expr _) =
        parenPrec p 25 $ text "(" <> pretty decl <> text ")"
                       <> prettyPrec 25 expr
    prettyPrec p (CUnary CPostIncOp expr _) =
        parenPrec p 26 $ prettyPrec 26 expr <> text "++"
    prettyPrec p (CUnary CPostDecOp expr _) =
        parenPrec p 26 $ prettyPrec 26 expr <> text "--"
    prettyPrec p (CUnary op expr _) =
        parenPrec p 25 $ pretty op <> prettyPrec 25 expr
    prettyPrec p (CSizeofExpr expr _) =
        parenPrec p 25 $ text "sizeof" <> text "(" <> pretty expr <> text ")"
    prettyPrec p (CSizeofType decl _) =
        parenPrec p 25 $ text "sizeof" <> text "(" <> pretty decl <> text ")"
    prettyPrec p (CAlignofExpr expr _) =
        parenPrec p 25 $ text "alignof" <> pretty expr
    prettyPrec p (CAlignofType decl _) =
        parenPrec p 25 $ text "alignof" <> pretty decl
    prettyPrec p (CIndex expr1 expr2 _) =
        parenPrec p 26 $ prettyPrec 26 expr1
                       <> text "[" <> pretty expr2 <> text "]"
    prettyPrec p (CCall expr args _) =
        parenPrec p 30 $ prettyPrec 30 expr <> text "("
            <> (sep . punctuate comma . map pretty) args <> text ")"
    prettyPrec p (CMember expr ident deref _) =
        parenPrec p 26 $ prettyPrec 26 expr
                       <> text (if deref then "->" else ".") <> identP ident
    prettyPrec p (CVar ident _) = identP ident
    prettyPrec p (CConst const _) = pretty const
    prettyPrec p (CCompoundLit decl initl _) =
        pretty decl <> hsep (map p initl) where
        p (desigs, init) = hsep (map pretty desigs) <> pretty init -- FIXME
    prettyPrec p (CStatExpr stat _) =
        text "({" <> pretty stat <> text "})" -- FIXME
    prettyPrec p (CLabAddrExpr ident _) = text "&" <> identP ident -- FIXME
    prettyPrec p (CBuiltinExpr _) = text "[[[builtin]]]"

instance Pretty CAssignOp where
    pretty CAssignOp = text "="
    pretty CMulAssOp = text "*="
    pretty CDivAssOp = text "/="
    pretty CRmdAssOp = text "%="
    pretty CAddAssOp = text "+="
    pretty CSubAssOp = text "-="
    pretty CShlAssOp = text "<<="
    pretty CShrAssOp = text ">>="
    pretty CAndAssOp = text "&="
    pretty CXorAssOp = text "^="
    pretty COrAssOp  = text "|="

instance Pretty CBinaryOp where
    pretty CMulOp = text "*"
    pretty CDivOp = text "/"
    pretty CRmdOp = text "%"
    pretty CAddOp = text "+"
    pretty CSubOp = text "-"
    pretty CShlOp = text "<<"
    pretty CShrOp = text ">>"
    pretty CLeOp  = text "<"
    pretty CGrOp  = text ">"
    pretty CLeqOp = text "<="
    pretty CGeqOp = text ">="
    pretty CEqOp  = text "=="
    pretty CNeqOp = text "!="
    pretty CAndOp = text "&"
    pretty CXorOp = text "^"
    pretty COrOp  = text "|"
    pretty CLndOp = text "&&"
    pretty CLorOp = text "||"

instance Pretty CUnaryOp where
    pretty CPreIncOp  = text "++"
    pretty CPreDecOp  = text "--"
    pretty CPostIncOp = text "++"
    pretty CPostDecOp = text "--"
    pretty CAdrOp     = text "&"
    pretty CIndOp     = text "*"
    pretty CPlusOp    = text "+"
    pretty CMinOp     = text "-"
    pretty CCompOp    = text "~"
    pretty CNegOp     = text "!"

instance Pretty CConst where
    pretty (CIntConst   int _) = text (show int)
    pretty (CCharConst  chr _) = text (show chr) -- FIXME: control characters
    pretty (CFloatConst flt _) = text flt
    pretty (CStrConst   str _) = text str

-- precedence of C operators
binPrec :: CBinaryOp -> Int
binPrec CMulOp = 20
binPrec CDivOp = 20
binPrec CRmdOp = 20
binPrec CAddOp = 19
binPrec CSubOp = 19
binPrec CShlOp = 18
binPrec CShrOp = 18
binPrec CLeOp  = 17
binPrec CGrOp  = 17
binPrec CLeqOp = 17
binPrec CGeqOp = 17
binPrec CEqOp  = 16
binPrec CNeqOp = 16
binPrec CAndOp = 15
binPrec CXorOp = 14
binPrec COrOp  = 13
binPrec CLndOp = 12
binPrec CLorOp = 11
