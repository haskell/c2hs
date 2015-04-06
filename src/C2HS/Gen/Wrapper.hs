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

-- friends
import C2HS.Gen.Monad (Wrapper(..))


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
    return $ DL.toList . DL.concat $ wraps


-- | Process a single fragment.
--
genWrapper :: Wrapper -> CST s (DList String)
genWrapper (Wrapper wfn ofn (CDecl specs [(Just decl, _, _)] _)
            args (boolres, boolargs) pos) = do
  let renamed = rename (internalIdent wfn) decl
  wrapdecl <- fixArgs ofn pos args boolargs renamed
  let fspecs = if boolres
               then map replaceBoolSpec specs
               else specs
  expr <- callBody ofn pos args decl
  let body = CCompound [] [CBlockStmt (CReturn (Just expr) undefNode)] undefNode
      wrapfn = CFunDef fspecs wrapdecl [] body undefNode
  return $ DL.fromList [render (pretty wrapfn) ++ "\n"]
genWrapper (Wrapper _ ofn _ _ _ pos) =
  internalWrapperErr pos ["genWrapper:" ++ ofn]

rename :: Ident -> CDeclr -> CDeclr
rename ide (CDeclr _ dds str attrs n) = CDeclr (Just ide) dds str attrs n

fixArgs :: String -> Position -> [Bool] -> [Bool] -> CDeclr
        -> CST s CDeclr
fixArgs ofn pos args bools (CDeclr ide fd str attrs n) = do
  fd' <- case fd of
    [] -> return []
    f:fs -> do
      f' <- fixFunArgs ofn pos args bools f
      return $ f' : fs
  return $ CDeclr ide fd' str attrs n

fixFunArgs :: String -> Position -> [Bool] -> [Bool] -> CDerivedDeclr
           -> CST s CDerivedDeclr
fixFunArgs ofn pos args bools
  (CFunDeclr (Right (adecls, flg)) attrs n) = do
  adecls' <- zipWithM (fixDecl ofn pos) (zip3 args bools [1..]) adecls
  return $ CFunDeclr (Right (adecls', flg)) attrs n
fixFunArgs ofn pos args bools cdecl =
  internalWrapperErr pos ["fixFunArgs:" ++ ofn,
                          "args=" ++ show args,
                          "bools=" ++ show bools,
                          "cdecl=" ++ show cdecl]

replaceBool :: CDecl -> CDecl
replaceBool (CDecl spec ds n) = CDecl (map replaceBoolSpec spec) ds n

replaceBoolSpec :: CDeclSpec -> CDeclSpec
replaceBoolSpec (CTypeSpec (CBoolType tn)) = CTypeSpec (CIntType tn)
replaceBoolSpec t = t

fixDecl :: String -> Position -> (Bool, Bool, Int) -> CDecl -> CST s CDecl
fixDecl _ _   (False, True,  idx) d = return $ replaceBool $ fixEmpty d idx
fixDecl _ _   (False, False, idx) d = return $ fixEmpty d idx
fixDecl _ pos (True,  _,     idx) din = do
  let (CDecl specs [(Just decl, Nothing, Nothing)] n) = fixEmpty din idx
  decl' <- addPtr pos decl
  return $ CDecl specs [(Just decl', Nothing, Nothing)] n

fixEmpty :: CDecl -> Int -> CDecl
fixEmpty d@(CDecl _ [(Just _, Nothing, Nothing)] _) _ = d
fixEmpty (CDecl ss [] n) idx =
  let d = CDeclr (Just $ internalIdent $ "c2hs__dummy_arg_" ++ show idx) [] Nothing [] n
  in CDecl ss [(Just d, Nothing, Nothing)] n

addPtr :: Position -> CDeclr -> CST s CDeclr
addPtr _ (CDeclr ide [] cs attrs n) =
  return $ CDeclr ide [CPtrDeclr [] n] cs attrs n
addPtr pos cdecl = if isPtrDeclr cdecl
                   then wrapperOnPointerErr pos
                   else invalidWrapperErr pos

callBody :: String -> Position -> [Bool] -> CDeclr -> CST s CExpr
callBody fn pos args (CDeclr _ (fd:_) _ _ n) = do
  as <- zipWithM (makeArg pos) (zip args [1..]) (funArgs fd)
  return $ CCall (CVar (internalIdent fn) n) as n


makeArg :: Position -> (Bool, Int) -> CDecl -> CST s CExpr
makeArg _ (arg, _) (CDecl _ [(Just (CDeclr (Just i) _ _ _ _), _, _)] n) =
  return $ case arg of
    False -> CVar i n
    True -> CUnary CIndOp (CVar i n) n
makeArg _ (arg, idx) (CDecl _ [] n) =
  let i = internalIdent $ "c2hs__dummy_arg_" ++ show idx
  in return $ case arg of
    False -> CVar i n
    True -> CUnary CIndOp (CVar i n) n
makeArg pos (arg, idx) cdecl =
  internalWrapperErr pos ["makeArg:arg=" ++ show arg,
                          "cdecl=" ++ show cdecl,
                          "idx=" ++ show idx]

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
