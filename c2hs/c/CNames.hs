--  C->Haskell Compiler: C name analysis
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 October 99
--
--  Version $Revision: 1.8 $ from $Date: 2004/06/11 07:10:16 $
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

module CNames (nameAnalysis)
where

import Monad	 (mapM_)

import Position  (Position, posOf)
import Idents	 (Ident, identToLexeme)

import C2HSState (CST, nop)
import CAST
import CAttrs    (AttrC, CObj(..), CTag(..), CDef(..))
import CBuiltin  (builtinTypeNames)
import CTrav     (CT, getCHeaderCT, runCT, enter, enterObjs, leave, leaveObjs,
		  ifCTExc, raiseErrorCTExc, defObj, findTypeObj, findValueObj,
		  defTag, refersToDef, isTypedef) 


-- monad and wrapper
-- -----------------

-- local instance of the C traversal monad
--
type NA a = CT () a

-- name analysis of C header files (EXPORTED)
--
nameAnalysis    :: AttrC -> CST s AttrC
nameAnalysis ac  = do
		     (ac', _) <- runCT naCHeader ac ()
		     return ac'


-- name analyis traversal
-- ----------------------

-- traverse a complete header file
--
-- * in case of an error, back off the current declaration
--
naCHeader :: NA ()
naCHeader  = do
	       -- establish definitions for builtins
	       --
	       mapM_ (uncurry defObjOrErr) builtinTypeNames
	       --
	       -- analyse the header
	       --
	       CHeader decls _ <- getCHeaderCT
	       mapM_ (\decl -> naCExtDecl decl `ifCTExc` nop) decls

-- Processing of toplevel declarations
--
-- * We turn function definitions into prototypes, as we are not interested in
--   function bodies.
--
naCExtDecl :: CExtDecl -> NA ()
naCExtDecl (CDeclExt decl                        ) = naCDecl decl
naCExtDecl (CFDefExt (CFunDef specs declr _ _ at)) = 
  naCDecl $ CDecl specs [(Just declr, Nothing, Nothing)] at
naCExtDecl (CAsmExt at                           ) = return ()

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
	mapMaybeM_ naCInit	  oinit
	mapMaybeM_ naCExpr	  oexpr

naCDeclSpec :: CDeclSpec -> NA ()
naCDeclSpec (CTypeSpec tspec) = naCTypeSpec tspec
naCDeclSpec _		      = nop

naCTypeSpec :: CTypeSpec -> NA ()
naCTypeSpec (CSUType   su   _) = naCStructUnion (StructUnionCT su) su
naCTypeSpec (CEnumType enum _) = naCEnum (EnumCT enum) enum
naCTypeSpec (CTypeDef  ide  _) = do
				   (obj, _) <- findTypeObj ide False
				   ide `refersToDef` ObjCD obj
naCTypeSpec _		       = nop

naCStructUnion :: CTag -> CStructUnion -> NA ()
naCStructUnion tag (CStruct _ oide decls _) =
  do
    mapMaybeM_ (`defTagOrErr` tag) oide
    enterObjs				-- enter local struct range for objects
    mapM_ naCDecl decls
    leaveObjs				-- leave range

naCEnum :: CTag -> CEnum -> NA ()
naCEnum tag enum@(CEnum oide enumrs _) =
  do
    mapMaybeM_ (`defTagOrErr` tag) oide
    mapM_ naEnumr enumrs
  where
    naEnumr (ide, oexpr) = do
			     ide `defObjOrErr` EnumCO ide enum
			     mapMaybeM_ naCExpr oexpr

naCDeclr :: CObj -> CDeclr -> NA ()
naCDeclr obj (CVarDeclr oide _) =
  mapMaybeM_ (`defObjOrErr` obj) oide
naCDeclr obj (CPtrDeclr _ declr _   ) =
  naCDeclr obj declr
naCDeclr obj (CArrDeclr declr oexpr _   ) =
  do
    naCDeclr obj declr
    mapMaybeM_ naCExpr oexpr
naCDeclr obj (CFunDeclr declr decls _ _ ) =
  do
    naCDeclr obj declr
    enterObjs				-- enter range of function arguments
    mapM_ naCDecl decls
    leaveObjs				-- end of function arguments

naCInit :: CInit -> NA ()
naCInit (CInitExpr expr  _) = naCExpr expr
naCInit (CInitList inits _) = mapM_ naCInit inits

naCExpr :: CExpr -> NA ()
naCExpr (CComma      exprs             _) = mapM_ naCExpr exprs
naCExpr (CAssign     _ expr1 expr2     _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCond       expr1 expr2 expr3 _) = naCExpr expr1 >> naCExpr expr2
					    >> naCExpr expr3
naCExpr (CBinary     _ expr1 expr2     _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCast       decl expr	       _) = naCDecl decl >> naCExpr expr
naCExpr (CUnary      _ expr	       _) = naCExpr expr
naCExpr (CSizeofExpr expr              _) = naCExpr expr
naCExpr (CSizeofType decl	       _) = naCDecl decl
naCExpr (CAlignofExpr expr             _) = naCExpr expr
naCExpr (CAlignofType decl	       _) = naCDecl decl
naCExpr (CIndex	      expr1 expr2      _) = naCExpr expr1 >> naCExpr expr2
naCExpr (CCall	      expr exprs       _) = naCExpr expr >> mapM_ naCExpr exprs
naCExpr (CMember      expr ide _       _) = naCExpr expr
naCExpr (CVar	      ide	       _) = do
					     (obj, _) <- findValueObj ide False
					     ide `refersToDef` ObjCD obj
naCExpr (CConst	      _		       _) = nop
naCExpr (CCompoundLit inits            _) = mapM_ naCInit inits


-- auxilliary functions
-- --------------------

-- raise an error and exception if the identifier is defined twice
--
defTagOrErr           :: Ident -> CTag -> NA ()
ide `defTagOrErr` tag  = do
			   otag <- ide `defTag` tag
			   case otag of
			     Nothing   -> nop
			     Just tag' -> declaredTwiceErr ide (posOf tag')

-- associate an object with a referring identifier
--
-- * currently, repeated declarations are completely ignored; eventually, the
--   consistency of the declarations should be checked
--
defObjOrErr           :: Ident -> CObj -> NA ()
ide `defObjOrErr` obj  = ide `defObj` obj >> nop

-- maps some monad operation into a `Maybe', discarding the result
--
mapMaybeM_ :: Monad m => (a -> m b) -> Maybe a -> m ()
mapMaybeM_ m Nothing   =        return ()
mapMaybeM_ m (Just a)  = m a >> return ()


-- error messages
-- --------------

declaredTwiceErr              :: Ident -> Position -> NA a
declaredTwiceErr ide otherPos  =
  raiseErrorCTExc (posOf ide) 
    ["Identifier declared twice!",
     "The identifier `" ++ identToLexeme ide ++ "' was already declared at " 
     ++ show otherPos ++ "."]
