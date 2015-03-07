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

import Control.Monad

-- Language.C / Compiler Toolkit
import Language.C.Syntax
import Language.C.Pretty
import Text.PrettyPrint.HughesPJ (render)
import Language.C.Data.Node (undefNode)
import Language.C.Data.Position
import Language.C.Data.Ident (Ident(..), internalIdent)
import Data.DList (DList)
import qualified Data.DList as DL

-- C->Haskell
import C2HS.State  (CST, raiseError, throwExc, catchExc,
                    errorsPresent, showErrors, fatal)
import C2HS.C.Trav (isPtrDeclr)
import Control.State (raiseError)

-- friends
import C2HS.Gen.Monad (Wrapper(..))

import Debug.Trace

-- | Generate a custom C wrapper from a CHS binding module for
-- functions that require marshalling of bare C structs.
--
genWrappers :: [Wrapper] -> CST s [String]
genWrappers ws = do
  wraps <- mapM genWrapper (reverse ws) `ifWrapExc` return []
  errs <- errorsPresent
  if errs
    then do
    errmsgs <- showErrors
    fatal ("Errors during generation of C wrappers:\n\n" ++ errmsgs)
    else do
    warnmsgs <- showErrors
    return $ DL.toList . DL.concat $ wraps


-- | Process a single fragment.
--
genWrapper :: Wrapper -> CST s (DList String)
genWrapper (Wrapper wfn ofn (CDecl specs [(Just decl, _, _)] _) args pos) = do
  let renamed = rename (internalIdent wfn) decl
  wrapdecl <- fixArgs ofn pos args renamed
  let wrapfn = CFunDef specs wrapdecl [] body undefNode
      body = CCompound [] [CBlockStmt (CReturn expr undefNode)] undefNode
      expr = Just $ callBody ofn args decl
  return $ DL.fromList [render (pretty wrapfn) ++ "\n"]
genWrapper (Wrapper _ ofn cdecl _ pos) =
  internalWrapperErr pos ["genWrapper:" ++ ofn]

rename :: Ident -> CDeclr -> CDeclr
rename ide (CDeclr _ dds str attrs n) = CDeclr (Just ide) dds str attrs n

fixArgs :: String -> Position -> [Bool] -> CDeclr -> CST s CDeclr
fixArgs ofn pos args (CDeclr ide fd str attrs n) = do
  fd' <- case fd of
    [] -> return []
    f:fs -> do
      f' <- fixFunArgs ofn pos args f
      return $ f' : fs
  return $ CDeclr ide fd' str attrs n
fixArgs ofn pos args cdecl =
  internalWrapperErr pos ["fixArgs:" ++ ofn,
                          "args=" ++ show args, "cdecl=" ++ show cdecl]

fixFunArgs :: String -> Position -> [Bool] -> CDerivedDeclr
           -> CST s CDerivedDeclr
fixFunArgs ofn pos args (CFunDeclr (Right (adecls, flg)) attrs n) = do
  adecls' <- zipWithM (fixDecl ofn pos) args adecls
  return $ CFunDeclr (Right (adecls', flg)) attrs n
fixFunArgs ofn pos args cdecl =
  internalWrapperErr pos ["fixFunArgs:" ++ ofn,
                          "args=" ++ show args,
                          "cdecl=" ++ show cdecl]

fixDecl :: String -> Position -> Bool -> CDecl -> CST s CDecl
fixDecl _ _ False d = return d
fixDecl _ pos True (CDecl specs [(Just decl, Nothing, Nothing)] n) = do
  decl' <- addPtr pos decl
  return $ CDecl specs [(Just decl', Nothing, Nothing)] n
fixDecl ofn pos arg cdecl =
  internalWrapperErr pos ["fixDecl:ofn",
                          "arg=" ++ show arg,
                          "cdecl=" ++ show cdecl]

addPtr :: Position -> CDeclr -> CST s CDeclr
addPtr _ (CDeclr ide [] cs attrs n) =
  return $ CDeclr ide [CPtrDeclr [] n] cs attrs n
addPtr pos cdecl = if isPtrDeclr cdecl
                   then wrapperOnPointerErr pos
                   else invalidWrapperErr pos

callBody :: String -> [Bool] -> CDeclr -> CExpr
callBody fn args (CDeclr _ (fd:_) _ _ n) =
   CCall (CVar (internalIdent fn) n) (zipWith makeArg args (funArgs fd)) n

makeArg :: Bool -> CDecl -> CExpr
makeArg arg (CDecl _ [(Just (CDeclr (Just i) _ _ _ _), _, _)] n) =
  case arg of
    False -> CVar i n
    True -> CUnary CIndOp (CVar i n) n

funArgs :: CDerivedDeclr -> [CDecl]
funArgs (CFunDeclr (Right (adecls, _)) _ _) = adecls

throwWrapExc :: CST s a
throwWrapExc = throwExc "wrapExc" "Error during wrapper generation"

ifWrapExc :: CST s a -> CST s a -> CST s a
ifWrapExc m handler  = m `catchExc` ("wrapExc", const handler)

raiseErrorWrapper :: Position -> [String] -> CST s a
raiseErrorWrapper pos errs = raiseError pos errs >> throwWrapExc

internalWrapperErr :: Position -> [String] -> CST s a
internalWrapperErr pos msg  =
  raiseErrorWrapper pos $
    ["Internal wrapper error!",
     "Something went wrong generating a bare structure wrapper."] ++ msg

wrapperOnPointerErr :: Position -> CST s a
wrapperOnPointerErr pos  =
  raiseErrorWrapper pos $
    ["Bare structure wrapper error!",
     "Are you trying to put a wrapper on a pointer type?"]

invalidWrapperErr :: Position -> CST s a
invalidWrapperErr pos  =
  raiseErrorWrapper pos $
    ["Bare structure wrapper error!",
     "Invalid bare structure wrapper"]
