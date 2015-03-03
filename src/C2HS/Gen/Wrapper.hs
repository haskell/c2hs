--  C->Haskell Compiler: custom wrapper generator
--
--  Author : Manuel M T Chakravarty
--  Created: 5 February 2003
--
--  Copyright (c) 2004 Manuel M T Chakravarty
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
--  This module implements the generation of a custom C file to wrap
--  functions requiring marshalling of bare C structs to pointers.
--

module C2HS.Gen.Wrapper (
  genWrappers
) where

-- Language.C / Compiler Toolkit
import Language.C.Syntax
import Language.C.Pretty
import Text.PrettyPrint.HughesPJ (render)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Ident (Ident(..), internalIdent)
import Data.DList (DList)
import qualified Data.DList as DL

-- C->Haskell
import C2HS.State (CST)

-- friends
import C2HS.Gen.Monad (Wrapper(..))


-- | Generate a custom C wrapper from a CHS binding module for
-- functions that require marshalling of bare C structs.
--
genWrappers :: [Wrapper] -> CST s [String]
genWrappers ws = return $ DL.toList $ DL.concat $ map genWrapper $ reverse ws


-- | Process a single fragment.
--
genWrapper :: Wrapper -> DList String
genWrapper (Wrapper wfn ofn (CDecl specs [(Just decl, _, _)] _) args) =
  DL.fromList [render (pretty wrapfn) ++ "\n"]
  where wrapfn = CFunDef specs wrapdecl [] body undefNode
        wrapdecl = fixArgs args $ rename (internalIdent wfn) decl
        body = CCompound [] [CBlockStmt (CReturn expr undefNode)] undefNode
        expr = Just $ callBody ofn args decl
genWrapper (Wrapper _ ofn _ _) = error $ "WRAPPER BORKED FOR " ++ ofn

rename :: Ident -> CDeclarator a -> CDeclarator a
rename ide (CDeclr _ dds str attrs n) = CDeclr (Just ide) dds str attrs n

fixArgs :: Show a => [Bool] -> CDeclarator a -> CDeclarator a
fixArgs args (CDeclr ide [fd] str attrs n) = CDeclr ide [fd'] str attrs n
  where fd' = fixFunArgs args fd
fixArgs args cdecl = error $ "NON-EXHAUSTIVE PATTERN IN fixArgs\n" ++
                     "args=" ++ show args ++ "\ncdecl=" ++ show cdecl

fixFunArgs :: Show a => [Bool] -> CDerivedDeclarator a -> CDerivedDeclarator a
fixFunArgs args (CFunDeclr (Right (adecls, flg)) attrs n) =
  CFunDeclr (Right (adecls', flg)) attrs n
  where adecls' = zipWith fixDecl args adecls
fixFunArgs args cdecl = error $ "NON-EXHAUSTIVE PATTERN IN fixFunArgs\n" ++
                     "args=" ++ show args ++ "\ncdecl=" ++ show cdecl

fixDecl :: Show a => Bool -> CDeclaration a -> CDeclaration a
fixDecl False d = d
fixDecl True (CDecl specs [(Just decl, Nothing, Nothing)] n) =
  CDecl specs [(Just decl', Nothing, Nothing)] n
  where decl' = addPtr decl
fixDecl arg cdecl = error $ "NON-EXHAUSTIVE PATTERN IN fixDecl\n" ++
                    "arg=" ++ show arg ++ "\ncdecl=" ++ show cdecl

addPtr :: Show a => CDeclarator a -> CDeclarator a
addPtr (CDeclr ide [] cs attrs n) =
  CDeclr ide [CPtrDeclr [] n] cs attrs n
addPtr cdecl = error $ "NON-EXHAUSTIVE PATTERN IN addPtr\n" ++
               "cdecl=" ++ show cdecl

callBody :: String -> [Bool] -> CDeclarator a -> CExpression a
callBody fn args (CDeclr _ [fd] _ _ n) =
   CCall (CVar (internalIdent fn) n) (zipWith makeArg args (funArgs fd)) n

makeArg :: Bool -> CDeclaration a -> CExpression a
makeArg arg (CDecl _ [(Just (CDeclr (Just i) _ _ _ _), _, _)] n) =
  case arg of
    False -> CVar i n
    True -> CUnary CIndOp (CVar i n) n

funArgs :: CDerivedDeclarator a -> [CDeclaration a]
funArgs (CFunDeclr (Right (adecls, _)) _ _) = adecls
