--  C->Haskell Compiler: C name analysis
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 October 99
--
--  Copyright (c) 1999 Manuel M. T. Chakravarty
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
--  Name analysis of C header files.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * Member names are not looked up, because this requires type information
--    about the expressions before the `.' or `->'.
--
--- TODO ----------------------------------------------------------------------
--
-- * `defObjOrErr': currently, repeated declarations are completely ignored;
--   eventually, the consistency of the declarations should be checked
--

module C2HS.C.Names (nameAnalysis)
where

import Language.C.Data.Ident
import Language.C.Data.Position
import Language.C.Syntax


import C2HS.State (CST)
import C2HS.C.Attrs    (AttrC, emptyAttrC, CObj(..), CTag(..), CDef(..))
import C2HS.C.Trav     (CT, runCT, enterObjs, leaveObjs,
                  ifCTExc, raiseErrorCTExc, defObj, findTypeObj, findValueObj,
                  defTag, refersToDef, isTypedef)
import C2HS.C.Builtin


-- monad and wrapper
-- -----------------

-- | local instance of the C traversal monad
--
type NA a = CT () a

-- | name analysis of C header files
--
nameAnalysis         :: CTranslUnit -> CST s AttrC
nameAnalysis headder  = do
                          (ac', _) <- runCT (naCTranslUnit headder) emptyAttrC ()
                          return ac'


-- name analyis traversal
-- ----------------------

-- | traverse a complete header file
--
-- * in case of an error, back off the current declaration
--
naCTranslUnit :: CTranslUnit -> NA ()
naCTranslUnit (CTranslUnit decls _) = do
               -- establish definitions for builtins
               --
               mapM_ (uncurry defObjOrErr) builtinTypeNames
               --
               -- analyse the header
               --
               mapM_ (\decl -> naCExtDecl decl `ifCTExc` return ()) decls

-- | Processing of toplevel declarations
--
-- * We turn function definitions into prototypes, as we are not interested in
--   function bodies.
--
naCExtDecl :: CExtDecl -> NA ()
naCExtDecl (CDeclExt decl                        ) = naCDecl decl
naCExtDecl (CFDefExt (CFunDef specs declr _ _ at)) =
  naCDecl $ CDecl specs [(Just declr, Nothing, Nothing)] at
naCExtDecl _                                       = return ()

naCDecl :: CDecl -> NA ()
naCDecl decl@(CDecl specs decls _) =
  do
    mapM_ naCDeclSpec specs
    mapM_ naTriple decls
  where
    naTriple (odeclr, oinit, oexpr) =
      do
        let obj = if isTypedef decl then TypeCO decl else ObjCO decl
        mapMaybeM_ (naCDeclr obj) odeclr
        mapMaybeM_ naCInit        oinit
        mapMaybeM_ naCExpr        oexpr

naCDeclSpec :: CDeclSpec -> NA ()
naCDeclSpec (CTypeSpec tspec) = naCTypeSpec tspec
naCDeclSpec _                 = return ()

naCTypeSpec :: CTypeSpec -> NA ()
naCTypeSpec (CSUType   su   _) = naCStructUnion (StructUnionCT su) su
naCTypeSpec (CEnumType enum _) = naCEnum (EnumCT enum) enum
naCTypeSpec (CTypeDef  ide  _) = do
                                   (obj, _) <- findTypeObj ide False
                                   ide `refersToDef` ObjCD obj
naCTypeSpec _                  = return ()

naCStructUnion :: CTag -> CStructUnion -> NA ()
naCStructUnion tag (CStruct _ oide decls _ _) =
  do
    mapMaybeM_ (`defTagOrErr` tag) oide
    enterObjs                           -- enter local struct range for objects
    mapM_ naCDecl (maybe [] id decls)
    leaveObjs                           -- leave range

naCEnum :: CTag -> CEnum -> NA ()
naCEnum tag enum@(CEnum oide enumrs _ _) =
  do
    mapMaybeM_ (`defTagOrErr` tag) oide
    mapM_ naEnumr (maybe [] id enumrs)
  where
    naEnumr (ide, oexpr) = do
                             ide `defObjOrErr` EnumCO ide enum
                             mapMaybeM_ naCExpr oexpr

-- Name analysis of a declarator
-- The derivations are analysed in order, only THEN
-- the object itself is entered into the symboltable
naCDeclr :: CObj -> CDeclr -> NA ()
naCDeclr obj (CDeclr oide derived _ _ _) =
  do
    mapM_ (naCDerivedDeclr obj) derived
    mapMaybeM_ (`defObjOrErr` obj) oide
naCDerivedDeclr :: CObj -> CDerivedDeclr -> NA ()
naCDerivedDeclr _obj (CFunDeclr (Right (params,_)) _ _) =
  do
    enterObjs
    mapM_ naCDecl params
    leaveObjs
naCDerivedDeclr _obj (CArrDeclr _ (CArrSize _ expr) _) =
  naCExpr expr
naCDerivedDeclr _obj _ = return ()

naCInit :: CInit -> NA ()
naCInit (CInitExpr expr  _) = naCExpr expr
naCInit (CInitList inits _) = mapM_ (naCInit . snd) inits

naCExpr :: CExpr -> NA ()
naCExpr (CComma      exprs             _) = mapM_ naCExpr exprs
naCExpr (CAssign     _ expr1 expr2     _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCond       expr1 expr2 expr3 _) = naCExpr expr1 >> mapMaybeM_ naCExpr expr2
                                            >> naCExpr expr3
naCExpr (CBinary     _ expr1 expr2     _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCast       decl expr         _) = naCDecl decl >> naCExpr expr
naCExpr (CUnary      _ expr            _) = naCExpr expr
naCExpr (CSizeofExpr expr              _) = naCExpr expr
naCExpr (CSizeofType decl              _) = naCDecl decl
naCExpr (CAlignofExpr expr             _) = naCExpr expr
naCExpr (CAlignofType decl             _) = naCDecl decl
naCExpr (CIndex       expr1 expr2      _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCall        expr exprs       _) = naCExpr expr >> mapM_ naCExpr exprs
naCExpr (CMember      expr _ide _      _) = naCExpr expr
naCExpr (CVar         ide              _) = do
                                             (obj, _) <- findValueObj ide False
                                             ide `refersToDef` ObjCD obj
naCExpr (CConst       _                 ) = return ()
naCExpr (CCompoundLit _ inits          _) = mapM_ (naCInit . snd) inits
naCExpr (CComplexImag expr             _) = naCExpr expr
naCExpr (CComplexReal expr             _) = naCExpr expr
naCExpr (CLabAddrExpr _lab             _) = error "Names.hs: adress of label expression analysis isn't supported"
naCExpr (CBuiltinExpr _                 ) = error "Names.hs: builtin expression analysis isn't supported"
naCExpr (CStatExpr _                   _) = error "Names.hs: analysis of GNU statement - expression isn't supported"
-- auxilliary functions
-- --------------------

-- | raise an error and exception if the identifier is defined twice
defTagOrErr           :: Ident -> CTag -> NA ()
ide `defTagOrErr` tag  = do
                           otag <- ide `defTag` tag
                           case otag of
                             Nothing   -> return ()
                             Just tag' -> declaredTwiceErr ide (posOf tag')

-- | associate an object with a referring identifier
--
-- * currently, repeated declarations are completely ignored; eventually, the
--   consistency of the declarations should be checked
--
defObjOrErr           :: Ident -> CObj -> NA ()
ide `defObjOrErr` obj  = ide `defObj` obj >> return ()

-- | maps some monad operation into a 'Maybe', discarding the result
--
mapMaybeM_ :: Monad m => (a -> m b) -> Maybe a -> m ()
mapMaybeM_ _ Nothing   =        return ()
mapMaybeM_ m (Just a)  = m a >> return ()


-- error messages
-- --------------

declaredTwiceErr              :: Ident -> Position -> NA a
declaredTwiceErr ide otherPos  =
  raiseErrorCTExc (posOf ide)
    ["Identifier declared twice!",
     "The identifier `" ++ identToString ide ++ "' was already declared at "
     ++ show otherPos ++ "."]
