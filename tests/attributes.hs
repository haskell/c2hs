--  The HiPar Toolkit: test routines for attribute management
--
--  Author : Manuel M.T. Chakravarty
--  Created: 9 November 1995
--
--  Copyright (C) [1995..1998] Manuel M. T. Chakravarty
--
--  THIS FILE IS DISTRIBUTED UNDER THE TERMS OF THE GNU PUBLIC LICENCE
--  NO WARRANTY WHATSOEVER IS PROVIDED
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module provides code used to test the `Attributes' module.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 1.4
--
--- TODO ----------------------------------------------------------------------
--

module Main
where

import Position
import Errors
import UNames
import NameSpaces
import Idents
import State
import Attributes


main :: IO ()
main  = run ("", "", "") () (
	  putStrCIO "HiPar attribute test program\n"		+>
	  putStrCIO "============================\n\n"		+>
	  getNameSupply						+>= \supply ->
	  testAttrTables (names supply)				+>
--
-- the following tests must lead to an internal error (violation of an
-- assertion) when any of them is executed
--
	  getNameSupply						+>= \supply ->
	  -- acces with uninit. attr.s
--	  checkAttrTableErr1 (names supply)			+>
	  -- set already set attr.
--	  checkAttrTableErr2 (names supply)			+>
	  -- write frozen table
--	  checkAttrTableErr3 (names supply)			+>
	  -- freeze frozen table
--	  checkAttrTableErr4 (names supply)			+>
	  testAttrWithMonad
	)

-- test the attribute table functions
-- ----------------------------------

data T = A | B | NoT | DontCareT 
       deriving (Eq, Show)

instance Attr T where

  undef = NoT

  isUndef NoT = True
  isUndef _   = False

  dontCare = DontCareT

  isDontCare DontCareT = True
  isDontCare _	       = False

at :: AttrTable T 
at  = newAttrTable "test table"

testAttrTables    :: [Name] -> PreCST e s ()
testAttrTables ns  = 
  putStrCIO "Checking the functions for attribute tables...\n"		+>
  let
    n10 : n13 : _ = ns
    aid10	  = newAttrs nopos n10
    aid13	  = newAttrs nopos n13
  in
  let
    ok1 = isUndef (at `getAttr` aid10)
    at2 = setAttr at aid10 A
    ok2 = (at2 `getAttr` aid10) == A
    at3 = updAttr at aid10 B
    ok3 = (at3 `getAttr` aid10) == B
    at4 = freezeAttrTable at3
    ok4 = (at4 `getAttr` aid10) == B
    ok5 = isUndef (at4 `getAttr` aid13)
    at6 = softenAttrTable at4
    ok6 = (at6 `getAttr` aid10) == B
    at7 = setAttr at6 aid13 A
    ok7 = (at7 `getAttr` aid13) == A
    ok8 = (at7 `getAttr` aid10) == B
  in
  if ok1 && ok2 && ok3 && ok4 && ok5 && ok6 && ok7 && ok8
  then
    putStrCIO "...they are ok.\n"		  
  else
    putStrCIO "...ERROR DETECTED: test(s) "                +>
    putIfNotOk "ok1" ok1				   +>
    putIfNotOk "ok2" ok2				   +>
    putIfNotOk "ok3" ok3				   +>
    putIfNotOk "ok4" ok4				   +>
    putIfNotOk "ok5" ok5				   +>
    putIfNotOk "ok6" ok6				   +>
    putIfNotOk "ok7" ok7				   +>
    putIfNotOk "ok8" ok8				   +>
    putStrCIO " failed.\n"
  where
    putIfNotOk name True  = yield ()
    putIfNotOk name False = putStrCIO name +> putStrCIO " "


-- tests that must lead to an internal error (violate some assertion)
--

checkAttrTableErr1 _ = putStrCIO (show (
		          at `getAttr` (newAttrsOnlyPos nopos)
		        ))
checkAttrTableErr2 ns = let
			  aid10 = newAttrs nopos (head ns)
			in
			putStrCIO (show (
			  setAttr (setAttr at aid10 A) aid10 A `getAttr` aid10
			))
checkAttrTableErr3 ns = let
			  aid10 = newAttrs nopos (head ns)
			in
			putStrCIO (show (
			  setAttr (freezeAttrTable at) aid10 A `getAttr` aid10
			))
checkAttrTableErr4 ns = let
			  aid10 = newAttrs nopos (head ns)
			in
			putStrCIO (show (
			  freezeAttrTable (freezeAttrTable at) `getAttr` aid10
			))


-- test attributes with monad
-- --------------------------

type VName = Ident

data Exp = Const Int
	 | Var   VName
	 | Add   Exp Exp
	 | Mul   Exp Exp
	 | Let   [Bd] Exp
--	 deriving (Show)

data Bd  = Bd VName Exp
--	 deriving (Show)

data A a = Undef
	 | DontCare
	 | Value a

instance Attr (A a) where
  undef    = Undef
  dontCare = DontCare

  isUndef Undef = True
  isUndef _     = False

  isDontCare DontCare = True
  isDontCare _        = False

value           :: A a -> a
value (Value x)  = x

type VarObj    = Bd
type VarsTable = AttrTable (A VarObj)
type MyState   = ([Name], VarsTable, NameSpace VarObj)
type MyCST a   = PreCST () MyState a

-- monad operation for the creation of attribute identifiers
--
enableAttrs    :: Ident -> MyCST Ident
enableAttrs id  = transCST enable
		  where
		    enable (n:ns, vt, env) = 
		      let
			id' = lexemeToIdent (posOf id) (identToLexeme id) n
		      in
			((ns, vt, env), id')

-- monad operations for the attribute table
--

getDefOf    :: Ident -> MyCST Bd
getDefOf id  = readCST (\(_, vt, _) -> value ((getAttr vt . getIdentAttrs) id))

becomesDef       :: Ident -> Bd -> MyCST ()
becomesDef id bd  = transCST (\(ais, vt, env) 
			      -> ((ais, 
				   setAttr vt (getIdentAttrs id) (Value bd), 
				   env), 
				  ()))

-- monad operations for environment management
--

enter :: MyCST ()
enter  = transCST (\(ais, vt, env) -> ((ais, vt, enterNewRange env), ()))

leave :: MyCST ()
leave  = transCST (\(ais, vt, env) -> ((ais, vt, fst (leaveRange env)), ()))

define         :: VName -> VarObj -> MyCST ()
define name vo  = transCST (\(ais, vt, env) 
			    -> let
				 (env', clash) = defLocal env name vo
			       in
				 case clash of
				   Nothing -> ((ais, vt, env'), ())
				   Just _  -> interr "Main: Identifier\
						     \ declared twice!")

findName      :: VName -> MyCST (Maybe VarObj)
findName name  = readCST (\(ais, vt, env) -> find env name)

-- start a monad-based traversal
--
runMyCST   :: MyCST a -> PreCST () s' a
runMyCST m  = do
	        supply <- getNameSupply
	        runCST m (names supply, vars, nameSpace)
	      where
	        vars = newAttrTable "variable objects"

-- simple expression
--
expr :: Exp
expr  = Let [Bd (onlyPosIdent nopos "x") (Const 1),
	     Bd (onlyPosIdent nopos "y") (Mul (Const 2) (Const 3))
	    ]
	    (Add (Var (onlyPosIdent nopos "x"))  
		 (Var (onlyPosIdent nopos "y")))

-- attribute an expression, i.e., create the attribute identifiers and perform
-- the name analysis
--
attr              :: Exp -> MyCST Exp
attr (Const v)     = yield (Const v)
attr (Var   n)	   = enableAttrs n		+>= \n' ->
		     findName n'		+>= \maybeobj ->
		     case maybeobj of
		       Nothing  -> interr "Main: Undeclared identifier!"
		       Just obj -> n' `becomesDef` obj	+>
				   yield (Var n')
attr (Add   e1 e2) = attr e1			+>= \e1' ->
		     attr e2			+>= \e2' ->
		     yield (Add e1' e2')
attr (Mul   e1 e2) = attr e1			+>= \e1' ->
		     attr e2			+>= \e2' ->
		     yield (Mul e1' e2')
attr (Let   bds e) = mapM attrBd bds		+>= \bds' ->
		     enter			+>
		       mapM def bds'		+>= \_	 ->
		       attr e			+>= \e'  ->
		     leave			+>
		     yield (Let bds' e')
		     where
		       def bd@(Bd name _) = define name bd

attrBd             :: Bd -> MyCST Bd
attrBd (Bd name e)  = attr e                     +>= \e' ->
		      enableAttrs name		 +>= \name' ->
		      let
			bd = Bd name' e'
		      in
		      name' `becomesDef` bd +>
		      yield bd

-- yields the value of an attributed expression
--
eval              :: Exp -> MyCST Int
eval (Const v    ) = yield v
eval (Var   n    ) = getDefOf n	        +>= \(Bd _ e) ->
		     eval e
eval (Add   e1 e2) = eval e1		+>= \v1 ->
		     eval e2		+>= \v2 ->
		     yield (v1 + v2)
eval (Mul   e1 e2) = eval e1		+>= \v1 ->
		     eval e2		+>= \v2 ->
		     yield (v1 * v2)
eval (Let   bds e) = eval e

testAttrWithMonad :: PreCST () s ()
testAttrWithMonad  = putStrCIO "Attributing simple expression...\n"  +>
		     runMyCST (
		       attr expr				     +>= \e' ->
		       putStrCIO "...evaluating attributed\
			         \ expression...\n"		     +>
		       eval e'
		     )						     +>= \v ->
		     putStrCIO ("...result is " ++ show v ++ " (should be 7).\n")
