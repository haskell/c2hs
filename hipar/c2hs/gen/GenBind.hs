--  C->Haskell Compiler: binding generator
--
--  Author : Manuel M. T. Chakravarty
--  Created: 17 August 99
--
--  Version $Revision: 1.19 $ from $Date: 2001/02/04 14:59:01 $
--
--  Copyright (c) [1999..2001] Manuel M. T. Chakravarty
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
--  Module implementing the expansion of the binding hooks.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * If there is an error in one binding hook, it is skipped and the next one 
--    is processed (to collect as many errors as possible).  However, if at
--    least one error occured, the expansion of binding hooks ends in a fatal
--    exception.
--
--  * `CST' exceptions are used to back off a binding hook as soon as an error 
--    is encountered while it is processed.
--
--  Mapping of C types to Haskell FFI types:
--  ----------------------------------------
--
--  The following defines the mapping for basic types.  If the type specifer
--  is missing, it is taken to be `int'.  In the following, elements enclosed
--  in square brackets are optional.
--
--    void                      -> ()
--    char		        -> CChar
--    unsigned char             -> CUChar
--    signed char               -> CShort
--    signed		        -> CInt
--    [signed] int              -> CInt
--    [signed] short [int]      -> CSInt
--    [signed] long [int]       -> CLong
--    [signed] long long [int]  -> CLLong
--    unsigned [int]		-> CUInt
--    unsigned short [int]	-> CUShort
--    unsigned long [int]	-> CULong
--    unsigned long long [int]	-> CULLong
--    float			-> CFloat
--    double			-> CDouble
--    long double		-> CLDouble
--    enum ...			-> CInt
--    struct ...		-> ** error **
--    union ...			-> ** error **
--
--  Plain structures or unions (ie, if not the base type of a pointer type)
--  are not supported at the moment (the underlying FFI does not support them
--  directly).  Named types (ie, in C type names defined using `typedef') are
--  traced back to their original definitions.  All pointer types are mapped
--  to `Addr'.
--
--  We obtain the size and alignment constraints for all primitive types of C
--  from `C2HSConfig' where the information is put by the GNU autoconf
--  generated `configure' script.
--
--  Identifier lookup:
--  ------------------
--
--  We allow to identify enumerations and structures by the names of `typedef' 
--  types aliased to them.
--
--  * enumerations: It is first checked whether there is a tag with the given
--      identifier; if such a tag does not exist, the definition of a typedef
--      with the same name is taken if it exists.
--  * structs/unions: like enumerations
--
--  We generally use `shadow' look ups.  When an identifier cannot be found,
--  we check whether - according to the prefix set by the context hook -
--  another identifier casts a shadow that matches.  If so, that identifier is 
--  taken instead of the original one.
--
--  Translation table handling for enumerators:
--  -------------------------------------------
--
--  First a translation table lookup on the original identifier of the
--  enumerator is done.  If that doesn't match and the prefix can be removed
--  from the identifier, a second lookup on the identifier without the prefix
--  is performed.  If this also doesn't match, the identifier without prefix
--  (possible after underscoreToCase translation is returned).  If there is a
--  match, the translation (without any further stripping of prefix) is
--  returned.  
--
--- TODO ----------------------------------------------------------------------
--
--  * A function prototype that uses a defined type on its left hand side may
--    declare a function, while that is not obvious from the declaration
--    itself (without also considering the `typedef').  Calls to such
--    functions are currently rejected, which is a BUG.
--
--  * context hook must be first
--
--  * Look up in translation tables is naive - this probably doesn't affect
--    costs much, but at some point a little profiling might be beneficial.
--
--  * The use of `++' in the recursive definition of the routines generating
--    `Enum' instances is not particularly efficient.
--
--  * Some operands are missing in `applyBin' - unfortunately, Haskell does
--    not have standard bit operations.   Some constructs are also missing
--    from `evalConstCExpr'.
--

module GenBind (expandHooks) 
where 

import Char	  (toUpper, toLower)
import List       (find, deleteBy)
import Maybe	  (isNothing, isJust, fromJust, fromMaybe, mapMaybe)
import Monad	  (when, liftM, mapAndUnzipM)
import Array	  (Array, (!))

import Common     (Position, Pos(posOf), nopos)
import Utils	  (lookupBy, mapMaybeM)
import Errors	  (interr, todo)
import Idents     (Ident, identToLexeme, onlyPosIdent)
import Attributes (newAttrsOnlyPos)

import C2HSConfig (dlsuffix)
import C2HSState  (CST, readCST, transCST, runCST, nop,
		   raiseError, errorsPresent, showErrors, fatal,
		   SwitchBoard(..), Traces(..), putTraceStr,
		   putStrCIO)
import C	  (AttrC, CObj(..), CTag(..), lookupDefObjC, lookupDefTagC,
		   CHeader(..), CExtDecl, CDecl(..), CDeclSpec(..),
		   CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
		   CStructUnion(..), CStructTag(..), CEnum(..), CDeclr(..),
		   CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..),
		   CUnaryOp(..), CConst (..),
		   CT, readCT, transCT, getCHeaderCT, runCT, ifCTExc,
		   raiseErrorCTExc, findFunObj, findTag,
		   applyPrefixToNameSpaces, isTypedef, simplifyDecl,
		   declrFromDecl, declrNamed, structMembers, structName,
		   isPtrType, structFromDecl, funResultAndArgs, chaseDecl,
		   findAndChaseDecl, checkForAlias, lookupEnum,
		   lookupStructUnion)
import CHS	  (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..),
		   CHSAccess(..), CHSAPath(..))
import CInfo      (CPrimType(..), sizes, alignments)


-- translation tables
-- ------------------

-- takes an identifier to a lexeme including a potential mapping by a
-- translation table
--
type TransFun = Ident -> String

-- translation function for the `underscoreToCase' flag
--
underscoreToCase     :: TransFun
underscoreToCase ide  = let lexeme = identToLexeme ide
			    ps	   = filter (not . null) . parts $ lexeme
			in
			concat . map adjustCase $ ps
			where
			  parts s = let (l, s') = break (== '_') s
				    in  
				    l : case s' of
					  []      -> []
					  (_:s'') -> parts s''
			  
			  adjustCase (c:cs) = toUpper c : map toLower cs

-- takes an identifier association table to a translation function
--
-- * if first argument is `True', identifiers that are not found in the
--   translation table are subjected to `underscoreToCase'
--
-- * the details of handling the prefix are given in the DOCU section at the
--   beginning of this file
--
transTabToTransFun :: String -> CHSTrans -> TransFun
transTabToTransFun prefix (CHSTrans _2Case table) =
  \ide -> let 
	    lexeme = identToLexeme ide
	    dft    = if _2Case			-- default uses maybe the...
		     then underscoreToCase ide  -- ..._2case transformed...
		     else lexeme		-- ...lexeme
	  in
	  case lookup ide table of		    -- lookup original ident
	    Just ide' -> identToLexeme ide'	    -- original ident matches
	    Nothing   -> 
	      case eat prefix lexeme of
	        Nothing          -> dft		    -- no match & no prefix
	        Just eatenLexeme -> 
		  let 
		    eatenIde = onlyPosIdent (posOf ide) eatenLexeme
		    eatenDft = if _2Case 
			       then underscoreToCase eatenIde 
			       else eatenLexeme
		  in
		  case lookup eatenIde table of     -- lookup without prefix
		    Nothing   -> eatenDft	    -- orig ide without prefix
		    Just ide' -> identToLexeme ide' -- without prefix matched
  where
    -- try to eat prefix and return `Just str' if successful
    --
    eat []         ('_':cs)                        = eat [] cs
    eat []         cs                              = Just cs
    eat (p:prefix) (c:cs) | toUpper p == toUpper c = eat prefix cs
			  | otherwise		   = Nothing
    eat _          _				   = Nothing


-- the local monad
-- ---------------

-- the local state consists of
--
-- (1) the dynamic library specified by the context hook,
-- (2) the prefix specified by the context hook,
-- (3) the set of delayed code fragaments, ie, pieces of Haskell code that,
--     finally, have to be appended at the CHS module together with the hook
--     that created them (the latter allows avoid duplication of foreign
--     export declarations)
--     
-- access to the attributes of the C structure tree is via the `CT' monad of
-- which we use an instance here
--
data GBState = GBState {
		 lib    :: String,		-- dynamic library
		 prefix :: String,		-- prefix
	         frags  :: [(CHSHook, CHSFrag)]	-- delayed code (with hooks)
	       }

type GB a = CT GBState a

initialGBState :: GBState
initialGBState  = GBState {
		    lib    = "",
		    prefix = "",
		    frags  = []
		  }

-- set the dynamic library and library prefix
--
setContext            :: (Maybe String) -> (Maybe String) -> GB ()
setContext lib prefix  = 
  transCT $ \state -> (state {lib    = fromMaybe "" lib,
			      prefix = fromMaybe "" prefix},
		       ())

-- get the dynamic library
--
getLibrary :: GB String
getLibrary  = readCT lib

-- get the prefix string
--
getPrefix :: GB String
getPrefix  = readCT prefix

-- add code to the delayed fragments (the code is made to start at a new line)
--
-- * currently only code belonging to call hooks can be delayed
--
-- * if code for the same call hook (ie, same C function) is delayed
--   repeatedly only the first entry is stored; it is checked that the hooks
--   specify the same flags (ie, produce the same delayed code)
--
delayCode          :: CHSHook -> String -> GB ()
delayCode hook str  = 
  do
    frags <- readCT frags
    frags' <- delay hook frags
    transCT (\state -> (state {frags = frags'}, ()))
    where
      newEntry = (hook, (CHSVerb ("\n" ++ str)))
      --
      delay hook@(CHSCall isFun isUns ide oalias _) frags =
	case find (\(hook', _) -> hook' == hook) frags of
	  Just (CHSCall isFun' isUns' ide' _ _, _) 
	    |    isFun == isFun' 
	      && isUns == isUns' 
	      && ide   == ide'   -> return frags
	    | otherwise		 -> err (posOf ide) (posOf ide')
	  Nothing                -> return $ frags ++ [newEntry]
      delay _ _                                  =
	interr "GenBind.delayCode: Illegal delay!"
      --
      err = incompatibleCallHooksErr

-- get the complete list of delayed fragments
--
getDelayedCode :: GB [CHSFrag]
getDelayedCode  = readCT (map snd . frags)


-- expansion of the hooks
-- ----------------------

-- given a C header file and a binding file, expand all hooks in the binding
-- file using the C header information (EXPORTED)
--
-- * If any error (not warnings) is encountered, a fatal error is raised.
--
-- * Also returns all warning messages encountered.
--
expandHooks        :: AttrC -> CHSModule -> CST s (CHSModule, String)
expandHooks ac mod  = do
		        (_, res) <- runCT (expandModule mod) ac initialGBState
			return res

expandModule		       :: CHSModule -> GB (CHSModule, String)
expandModule (CHSModule frags)  =
  do
    -- expand hooks
    --
    traceInfoExpand
    frags'       <- mapM expandFrag frags
    delayedFrags <- getDelayedCode

    -- check for errors and finalise
    --
    errs <- errorsPresent
    if errs
      then do
	traceInfoErr
	errmsgs <- showErrors
	fatal ("Errors during expansion of binding hooks:\n\n"   -- fatal error
	       ++ errmsgs)
      else do
	traceInfoOK
	warnmsgs <- showErrors
	return (CHSModule (frags' ++ delayedFrags), warnmsgs)
  where
    traceInfoExpand = putTraceStr tracePhasesSW 
			("...expanding binding hooks...\n")
    traceInfoErr    = putTraceStr tracePhasesSW 
			("...error(s) detected.\n")
    traceInfoOK     = putTraceStr tracePhasesSW 
			("...successfully completed.\n")

expandFrag :: CHSFrag -> GB CHSFrag
expandFrag verb@(CHSVerb _) = return verb
expandFrag      (CHSHook h) = liftM CHSVerb (expandHook h)
			      `ifCTExc` return (CHSVerb "** ERROR **")

expandHook :: CHSHook -> GB String
expandHook (CHSContext _ olib oprefix _) =
  do
    setContext olib oprefix		      -- enter context information
    mapMaybeM applyPrefixToNameSpaces oprefix -- use the prefix on name spaces
    return ""
expandHook (CHSType ide pos) =
  do
    decl <- findAndChaseDecl ide False True	-- no indirection, but shadows
    ty <- extractSimpleType pos decl
    return $ showExtType ty
expandHook (CHSEnum cide oalias chsTrans _) =
  do
    -- get the corresponding C declaration
    --
    enum <- lookupEnum cide True	-- smart lookup incl error handling
    --
    -- convert the translation table and generate data type definition code
    --
    prefix <- getPrefix
    let trans = transTabToTransFun prefix chsTrans
	hide  = identToLexeme . fromMaybe cide $ oalias
    enumDef cide enum hide trans
expandHook hook@(CHSCall isFun isUns ide oalias pos) =
  do
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used 
    -- afterwards instead of the original one
    --
    (cobj, ide) <- findFunObj ide True
    let ideLexeme = identToLexeme ide  -- orignal name might have been a shadow
	hsLexeme  = ideLexeme `maybe` identToLexeme $ oalias
    --
    -- compute the external type from the declaration, get the library, and
    -- delay the foreign export declaration
    --
    extType <- extractFunType pos ide cobj isFun
    lib     <- getLibrary
    delayCode hook (foreignImport lib ideLexeme hsLexeme isUns extType)
    return hsLexeme
expandHook (CHSField access path pos) =
  do
    (decl, offsets) <- accessPath path
    ty <- extractSimpleType pos decl
    setGet pos access offsets ty

-- produce code for an enumeration
--
-- * an extra instance declaration is required when any of the enumeration
--   constants is explicitly assigned a value in its definition
--
-- * the translation function strips prefixes where possible (different
--   enumerators maye have different prefixes)
--
enumDef :: Ident -> CEnum -> String -> TransFun -> GB String
enumDef _ cenum@(CEnum _ list _) hident trans =
  do
    (list', derived) <- evalTagVals list
    let enumVals = [(trans ide, cexpr) | (ide, cexpr) <-  list']  -- translate
        defHead  = enumHead hident
	defBody  = enumBody (length defHead - 2) enumVals
	inst	 = if derived then "deriving (Enum)"
		              else "\n" ++ enumInst hident enumVals
    return (defHead ++ defBody ++ inst)
  where
    cpos = posOf cenum
    --
    evalTagVals []                     = return ([], True)
    evalTagVals ((ide, Nothing ):list) = 
      do
        (list', derived) <- evalTagVals list
        return ((ide, Nothing):list', derived)
    evalTagVals ((ide, Just exp):list) = 
      do
        (list', derived) <- evalTagVals list
	val <- evalConstCExpr exp
	case val of
	  IntResult val' -> 
	    return ((ide, Just $ CConst (CIntConst val' at1) at2):list', 
		    False)
          FloatResult _ ->
	    illegalConstExprErr (posOf exp) "a float result"
      where
        at1 = newAttrsOnlyPos nopos
        at2 = newAttrsOnlyPos nopos

-- Haskell code for the head of an enumeration definition
--
enumHead       :: String -> String
enumHead ident  = "data " ++ ident ++ " = "

-- Haskell code for the body of an enumeration definition
--
enumBody                        :: Int -> [(String, Maybe CExpr)] -> String
enumBody indent []               = ""
enumBody indent ((ide, _):list)  =
  ide ++ "\n" ++ replicate indent ' ' 
  ++ (if null list then "" else "| " ++ enumBody indent list)

-- Haskell code for an instance declaration for `Enum'
--
-- * the expression of all explicitly specified tag values already have to be
--   in normal form, ie, to be an int constant
--
-- * enumerations start at 0 and whenever an explicit value is specified,
--   following tags are assigned values continuing from the explicitly
--   specified one
--
enumInst :: String -> [(String, Maybe CExpr)] -> String
enumInst ident list =
  "instance Enum " ++ ident ++ " where\n" 
  ++ fromDef list 0 ++ "\n" ++ toDef list 0
  where
    fromDef []                _ = ""
    fromDef ((ide, exp):list) n = 
      "  fromEnum " ++ ide ++ " = " ++ show' val ++ "\n" 
      ++ fromDef list (val + 1)
      where
        val = case exp of
		Nothing                         -> n
		Just (CConst (CIntConst m _) _) -> m
		Just _		                -> 
		  interr "GenBind.enumInst: Integer constant expected!"
	--
        show' x = if x < 0 then "(" ++ show x ++ ")" else show x
    --
    toDef []                _ = ""
    toDef ((ide, exp):list) n = 
      "  toEnum " ++ show' val ++ " = " ++ ide ++ "\n" 
      ++ toDef list (val + 1)
      where
        val = case exp of
		Nothing                         -> n
		Just (CConst (CIntConst m _) _) -> m
		Just _		                -> 
		  interr "GenBind.enumInst: Integer constant expected!"
	--
        show' x = if x < 0 then "(" ++ show x ++ ")" else show x

-- Haskell code for the foreign import declaration needed by a call hook
--
-- * appends a configuration dependent library suffix `dlsuffix'
--
foreignImport :: String -> String -> String -> Bool -> ExtType -> String
foreignImport lib ident hsIdent isUnsafe ty  =
  "foreign import ccall " ++ libName ++ "\"" ++ ident ++ "\"" ++ maybeUnsafe 
  ++ "\n  " ++ hsIdent ++ " :: " ++ showExtType ty ++ "\n"
  where
    libName	= if null lib then "" else "\"" ++ lib ++ dlsuffix ++ "\" "
    maybeUnsafe = if isUnsafe then " unsafe" else ""

-- compute from an access path, the declerator finally accessed and the index
-- path required for the access
--
-- * each element in the index path specifies dereferencing an address and the 
--   offset to be added to the address before dereferencing
--
-- * the returned declaration is already normalised (ie, alias have been
--   expanded) 
--
-- * it may appear as if `t.m' and `t->m' should have different access paths,
--   as the latter specifies one more dereferencing; this is certainly true in
--   C, but it doesn't apply here, as `t.m' is merely provided for the
--   convenience of the interface writer - it is strictly speaking an
--   impossible access paths, as in Haskell we always have a pointer to a
--   structure, we can never have the structure as a value itself
--
accessPath :: CHSAPath -> GB (CDecl, [Int])
accessPath (CHSRoot ide) =				-- t
  do
    decl <- findAndChaseDecl ide False True
    return (ide `simplifyDecl` decl, [0])
accessPath (CHSDeref (CHSRoot ide) _) =			-- *t
  do
    decl <- findAndChaseDecl ide True True
    return (ide `simplifyDecl` decl, [0])
accessPath (CHSRef root@(CHSRoot ide1) ide2) =		-- t.m
  do
    su <- lookupStructUnion ide1 False True
    (offset, decl') <- refStruct su ide2
    odecl <- checkForAlias decl'
    return (fromMaybe decl' odecl, [offset])
accessPath (CHSRef (CHSDeref (CHSRoot ide1) _) ide2) =	-- t->m
  do
    su <- lookupStructUnion ide1 True True
    (offset, decl') <- refStruct su ide2
    odecl <- checkForAlias decl'
    return (fromMaybe decl' odecl, [offset])
accessPath (CHSRef path ide) =				-- a.m
  do
    (decl, offset:offsets) <- accessPath path
    assertPrimDeclr ide decl
    su <- structFromDecl (posOf ide) decl
    (addOffset, decl') <- refStruct su ide
    odecl <- checkForAlias decl'
    return (fromMaybe decl' odecl, offset + addOffset : offsets)
  where
    assertPrimDeclr ide (CDecl _ [declr] _) =
      case declr of
        (Just (CVarDeclr _ _), _, _) -> nop
	_			     -> structExpectedErr ide
accessPath (CHSDeref path pos) =				-- *a
  do
    (decl, offsets) <- accessPath path
    decl' <- derefOrErr decl
    odecl <- checkForAlias decl'
    return (fromMaybe decl' odecl, 0 : offsets)
  where
    derefOrErr (CDecl specs [declr] at) =
      case declr of
        (Just (CPtrDeclr [_]       declr at), oinit, oexpr) -> 
	  return $ CDecl specs [(Just declr, oinit, oexpr)] at
        (Just (CPtrDeclr (_:quals) declr at), oinit, oexpr) -> 
	  return $ 
	    CDecl specs [(Just (CPtrDeclr quals declr at), oinit, oexpr)] at
	_			                            -> 
	  ptrExpectedErr pos

-- given a structure declaration and member name, compute the offset of the
-- member in the structure and the declaration of the referenced member
--
refStruct :: CStructUnion -> Ident -> GB (Int, CDecl)
refStruct su ide =
  do
    -- get the list of fields and check for our selector
    --
    let (fields, tag) = structMembers su
	(pre, post)   = span (not . flip declNamed ide) fields
    when (null post) $
      unknownFieldErr (posOf su) ide
    --
    -- get sizes of preceding fields and the result type (`pre' are all
    -- declarators preceding `ide' and the first declarator in `post' defines 
    -- `ide')
    --
    let decl = head post
    offset <- case tag of
		CStructTag -> offsetInStruct pre decl tag
		CUnionTag  -> return 0
    return (offset, decl)

-- does the given declarator define the given name?
--
declNamed :: CDecl -> Ident -> Bool
(CDecl _ [(Nothing   , _, _)] _) `declNamed` ide = False
(CDecl _ [(Just declr, _, _)] _) `declNamed` ide = declr `declrNamed` ide
(CDecl _ []                   _) `declNamed` _   =
  interr "GenBind.refStruct: Abstract declarator in structure!"
_				 `declNamed` _   =
  interr "GenBind.refStruct: More than one declarator!"

-- Haskell code for writing to or reading from a struct
--
setGet :: Position -> CHSAccess -> [Int] -> ExtType -> GB String
setGet pos access offsets ty =
  do
    let pre = case access of 
			  CHSSet -> "(\\adr val -> do {"
			  CHSGet -> "(\\adr -> do {"
    body <- setGetBody (reverse offsets)
    return $ pre ++ body ++ "})"
  where
    setGetBody [offset] =
      do
	typeTag <- accessType ty
	return $ case access of
	  CHSSet -> "assignOff_ adr " ++ show offset 
		    ++ " (val::" ++ typeTag ++ ")"
	  CHSGet -> "derefOff_ adr " ++ show offset ++ " ::IO " ++ typeTag
      where
	accessType (FunET      _   _) = return "Addr"
	accessType (IOET       _    ) = interr "GenBind.setGet: Illegal type!"
	accessType (DefinedET  ide _) = chaseDecl ide False   >>=
					extractSimpleType pos >>=
					accessType
	accessType (UnitET          ) = voidFieldErr pos
	accessType ty	              = return $ showExtType ty
    setGetBody (offset:offsets) =
      do
	code <- setGetBody offsets
	return $ "adr <- derefOff_ adr " ++ show offset ++ "::IO Addr;" ++ code


-- C code computations
-- -------------------

-- the result of a constant expression
--
data ConstResult = IntResult   Integer
		 | FloatResult Float

-- types that may occur in foreign declarations
--
-- * defined types are represented by their C idenifier and the Haskell name
--   used for them (after the application of possible translations)
--
data ExtType = FunET     ExtType ExtType	-- function
	     | IOET      ExtType		-- operation with side effect
	     | DefinedET Ident String		-- typedef'ed type
	     | PrimET    CPrimType		-- basic C type
	     | UnitET				-- void

-- composite C type
--
data CompType = ExtType  ExtType		-- external type
	      | SUType	 CStructUnion		-- structure or union

-- pretty print an external type
--
showExtType :: ExtType -> String
showExtType (FunET arg res)     = "(" ++ showExtType arg ++ " -> " 
			           ++ showExtType res ++ ")"
showExtType (IOET t)            = "IO " ++ showExtType t
showExtType (DefinedET _ str)   = str
showExtType (PrimET CAddrPT)    = "Addr"
showExtType (PrimET CCharPT)    = "CChar"
showExtType (PrimET CUCharPT)   = "CUChar"
showExtType (PrimET CSCharPT)   = "CSChar"
showExtType (PrimET CIntPT)     = "CInt"
showExtType (PrimET CShortPT)   = "CShort"
showExtType (PrimET CLongPT)    = "CLong"
showExtType (PrimET CLLongPT)   = "CLLong"
showExtType (PrimET CUIntPT)    = "CUInt"
showExtType (PrimET CUShortPT)  = "CUShort"
showExtType (PrimET CULongPT)   = "CULong"
showExtType (PrimET CULLongPT)  = "CULLong"
showExtType (PrimET CFloatPT)   = "CFloat"
showExtType (PrimET CDoublePT)  = "CDouble"
showExtType (PrimET CLDoublePT) = "CLDouble"
showExtType UnitET	        = "()"

-- compute the type of the C function declared by the given C object
--
-- * the identifier specifies in which of the declarators we are interested
--
-- * if the third argument is `True', the function result should not be
--   wrapped into an `IO' type
--
-- * the caller has to guarantee that the object does indeed refer to a
--   function 
--
extractFunType :: Position -> Ident -> CObj -> Bool -> GB ExtType
extractFunType pos ide (ObjCO cdecl) isFun = 
  do
    -- remove all declarators except that of the function we are processing;
    -- then, extract the functions arguments and result type (also check that
    -- the function is not variadic); finally, compute the external type for
    -- the result
    --
    let decl'                        = ide `simplifyDecl` cdecl
	(args, resultDecl, variadic) = funResultAndArgs decl'
    when variadic $
      variadicErr pos cpos
    preResultType <- extractSimpleType pos resultDecl
    --
    -- we can now add the `IO' monad if this is no pure function 
    --
    let resultType = if isFun 
		     then      preResultType 
		     else IOET preResultType
    --
    -- compute function arguments and create a function type (a function
    -- prototype with `void' as its single argument declares a nullary
    -- function) 
    --
    argTypes <- mapM (extractSimpleType pos) args
    case argTypes of
      [UnitET] -> return resultType
      _	       -> return (foldr FunET resultType argTypes)
  where
    cpos = posOf cdecl
extractFunType _ _ _ _ = 
  interr "GenBind.extractFunType: Illegal C object!"

-- compute a non-function and non-struct/union type from the given declaration 
--
-- * the declaration may have at most one declarator
--
-- * C functions are represented as `Addr'
--
extractSimpleType           :: Position -> CDecl -> GB ExtType
extractSimpleType pos cdecl  =
  do
    ct <- extractCompType cdecl
    case ct of
      ExtType et -> return et
      SUType  _  -> illegalStructUnionErr (posOf cdecl) pos

-- compute a non-function type from the given declaration
--
-- * the declaration may have at most one declarator
--
-- * C functions are represented as `Addr'
--
-- * typedef'ed types are chased
--
extractCompType :: CDecl -> GB CompType
extractCompType cdecl@(CDecl specs declrs _)
  | isPtr     = return $ ExtType (PrimET CAddrPT)
  | otherwise = do
		  oalias <- checkForAlias cdecl
		  case oalias of
		    Nothing    -> specType (posOf cdecl) specs
		    Just decl' -> extractCompType decl'
  where
    isPtr = case declrs of
	      []                   -> False
	      [(Just declr, _, _)] -> isPtrType declr
	      _		           -> moreThanOneDeclrErr
    moreThanOneDeclrErr = interr "GenBind.extractCompType: There was more \
				 \than one declarator!"

-- C to Haskell type mapping described in the DOCU section
--
typeMap :: [([CTypeSpec], ExtType)]
typeMap  = [([void]                      , UnitET           ),
	    ([char]			 , PrimET CCharPT   ),
	    ([unsigned, char]		 , PrimET CUCharPT  ),
	    ([signed, char]		 , PrimET CSCharPT  ),
	    ([signed]			 , PrimET CIntPT    ),
	    ([int]			 , PrimET CIntPT    ),
	    ([signed, int]		 , PrimET CIntPT    ),
	    ([short]			 , PrimET CShortPT  ),
	    ([short, int]		 , PrimET CShortPT  ),
	    ([signed, short]		 , PrimET CShortPT  ),
	    ([signed, short, int]        , PrimET CShortPT  ),
	    ([long]                      , PrimET CLongPT   ),
	    ([long, int]                 , PrimET CLongPT   ),
	    ([signed, long]              , PrimET CLongPT   ),
	    ([signed, long, int]         , PrimET CLongPT   ),
	    ([long, long]                , PrimET CLLongPT  ),
	    ([long, long, int]           , PrimET CLLongPT  ),
	    ([signed, long, long]        , PrimET CLLongPT  ),
	    ([signed, long, long, int]   , PrimET CLLongPT  ),
	    ([unsigned]			 , PrimET CUIntPT   ),
	    ([unsigned, int]		 , PrimET CUIntPT   ),
	    ([unsigned, short]		 , PrimET CUShortPT ),
	    ([unsigned, short, int]	 , PrimET CUShortPT ),
	    ([unsigned, long]		 , PrimET CULongPT  ),
	    ([unsigned, long, int]	 , PrimET CULongPT  ),
	    ([unsigned, long, long]	 , PrimET CULLongPT ),
	    ([unsigned, long, long, int] , PrimET CULLongPT ),
	    ([float]			 , PrimET CFloatPT  ),
	    ([double]			 , PrimET CDoublePT ),
	    ([long, double]		 , PrimET CLDoublePT),
	    ([enum]			 , PrimET CIntPT    )]
	   where
	     void     = CVoidType   undefined
	     char     = CCharType   undefined
	     short    = CShortType  undefined
	     int      = CIntType    undefined
	     long     = CLongType   undefined
	     float    = CFloatType  undefined
	     double   = CDoubleType undefined
	     signed   = CSignedType undefined
	     unsigned = CUnsigType  undefined
	     enum     = CEnumType   undefined undefined

-- compute the complex (external) type determined by a list of type specifiers
--
specType            :: Position -> [CDeclSpec] -> GB CompType
specType cpos specs  = 
  let tspecs = [ts | CTypeSpec ts <- specs]
  in case lookupTSpec tspecs typeMap of
    Just et -> if isUnsupportedType et
	         then unsupportedTypeSpecErr cpos
	         else return $ ExtType et
    Nothing -> case tspecs of
	         [CTypeDef  ide _] -> let name = identToLexeme ide 
				      in
				      return $ ExtType (DefinedET ide name)
	         [CSUType   cu  _] -> return $ SUType cu     -- struct or union
		 [CEnumType _   _] -> return $ ExtType (PrimET CIntPT)  -- enum
		 _		   -> illegalTypeSpecErr cpos
  where
    lookupTSpec = lookupBy matches
    --
    isUnsupportedType (PrimET et) = sizes!et == 0
    isUnsupportedType _		  = False
    --
    -- check whether twp type sepcifier lists denote the same type; handles
    -- types like `long long' correctly, as `deleteBy' removes only the first
    -- occurrence of the given element
    --
    matches :: [CTypeSpec] -> [CTypeSpec] -> Bool
    []           `matches` []     = True
    []           `matches` (_:_)  = False
    (spec:specs) `matches` specs' 
      | any (eqSpec spec) specs'  = specs `matches` deleteBy eqSpec spec specs'
      | otherwise		  = False
    --
    eqSpec (CVoidType   _) (CVoidType   _) = True
    eqSpec (CCharType   _) (CCharType   _) = True
    eqSpec (CShortType  _) (CShortType  _) = True
    eqSpec (CIntType    _) (CIntType    _) = True
    eqSpec (CLongType   _) (CLongType   _) = True
    eqSpec (CFloatType  _) (CFloatType  _) = True
    eqSpec (CDoubleType _) (CDoubleType _) = True
    eqSpec (CSignedType _) (CSignedType _) = True
    eqSpec (CUnsigType  _) (CUnsigType  _) = True
    eqSpec (CSUType   _ _) (CSUType   _ _) = True
    eqSpec (CEnumType _ _) (CEnumType _ _) = True
    eqSpec (CTypeDef  _ _) (CTypeDef  _ _) = True
    eqSpec _		   _		   = False

-- compute the offset of the declarator in the second argument when it is
-- preceded by the declarators in the first argument
--
offsetInStruct                :: [CDecl] -> CDecl -> CStructTag -> GB Int
offsetInStruct []    _    _    = return 0
offsetInStruct decls decl tag  = 
  do
    (offset, _) <- sizeAlignOfStruct decls tag
    (_, align)  <- sizeAlignOf decl
    return $ ((offset - 1) `div` align + 1) * align

-- compute the size and alignment (no padding at the end) of the declarators
-- forming a struct
--
sizeAlignOfStruct                  :: [CDecl] -> CStructTag -> GB (Int, Int)
sizeAlignOfStruct []    _           = return (0, 1)
sizeAlignOfStruct decls CStructTag  = 
  do
    (offset, preAlign) <- sizeAlignOfStruct (init decls) CStructTag
    (size, align)      <- sizeAlignOf       (last decls)
    let sizeOfStruct  = ((offset - 1) `div` align + 1) * align + size
	alignOfStruct = preAlign `max` align
    return (sizeOfStruct, alignOfStruct)
sizeAlignOfStruct decls CUnionTag   =
  do
    (sizes, aligns) <- mapAndUnzipM sizeAlignOf decls
    return (maximum sizes, maximum aligns)

-- compute the size and alignment constraint of a given C declaration
--
sizeAlignOf       :: CDecl -> GB (Int, Int)
sizeAlignOf cdecl  = 
  do
    ct <- extractCompType cdecl
    case ct of
      ExtType (FunET     _ _  ) -> return (sizes!CAddrPT, alignments!CAddrPT)
      ExtType (IOET      _    ) -> interr "GenBind.sizeof: Illegal IO type!"
      ExtType (DefinedET ide _) -> chaseDecl ide False >>= sizeAlignOf
      ExtType (PrimET    pt   ) -> return (sizes!pt, alignments!pt)
      ExtType (UnitET         ) -> voidFieldErr (posOf cdecl)
      SUType  su                -> 
        do
	  let (fields, tag) = structMembers su
	  fields' <- let ide = structName su 
		     in
		     if (not . null $ fields) || isNothing ide
		     then return fields
		     else do				  -- get the real...
		       tag <- findTag (fromJust ide)      -- ...definition
		       case tag of
			 Just (StructUnionCT su) -> return
						     (fst . structMembers $ su)
                         _                       -> return fields
	  sizeAlignOfStruct fields' tag

-- evaluate a constant expression
--
evalConstCExpr :: CExpr -> GB ConstResult
evalConstCExpr (CComma _ at) =
  illegalConstExprErr (posOf at) "a comma expression"
evalConstCExpr (CAssign _ _ _ at) =
  illegalConstExprErr (posOf at) "an assignment"
evalConstCExpr (CCond b t e _) =
  do
    bv <- evalConstCExpr b
    case bv of
      IntResult bvi  -> if bvi /= 0 then evalConstCExpr t else evalConstCExpr e
      FloatResult _ -> illegalConstExprErr (posOf b) "a float result"
evalConstCExpr (CBinary op lhs rhs at) =
  do
    lhsVal <- evalConstCExpr lhs
    rhsVal <- evalConstCExpr rhs
    let (lhsVal', rhsVal') = usualArithConv lhsVal rhsVal
    applyBin (posOf at) op lhsVal' rhsVal'
evalConstCExpr (CCast _ _ _) =
  todo "GenBind.evalConstCExpr: Casts are not implemented yet."
evalConstCExpr (CUnary op arg at) =
  do
    argVal <- evalConstCExpr arg
    applyUnary (posOf at) op argVal
evalConstCExpr (CSizeofExpr _ _) =
  todo "GenBind.evalConstCExpr: sizeof not implemented yet."
evalConstCExpr (CSizeofType _ _) =
  todo "GenBind.evalConstCExpr: sizeof not implemented yet."
evalConstCExpr (CIndex _ _ at) =
  illegalConstExprErr (posOf at) "array indexing"
evalConstCExpr (CCall _ _ at) =
  illegalConstExprErr (posOf at) "function call"
evalConstCExpr (CMember _ _ _ at) =
  illegalConstExprErr (posOf at) "a . or -> operator"
evalConstCExpr (CVar _ _ ) =
  todo "GenBind.evalConstCExpr: variable names not implemented yet."
evalConstCExpr (CConst c _) =
  evalCConst c

evalCConst :: CConst -> GB ConstResult
evalCConst (CIntConst   i _ ) = return $ IntResult i
evalCConst (CCharConst  c _ ) = return $ IntResult (toInteger (fromEnum c))
evalCConst (CFloatConst s _ ) = 
  todo "GenBind.evalCConst: Float conversion from literal misses."
evalCConst (CStrConst   s at) = 
  illegalConstExprErr (posOf at) "a string constant"

usualArithConv :: ConstResult -> ConstResult -> (ConstResult, ConstResult)
usualArithConv lhs@(FloatResult _) rhs                 = (lhs, toFloat rhs)
usualArithConv lhs                 rhs@(FloatResult _) = (toFloat lhs, rhs)
usualArithConv lhs                 rhs                 = (lhs, rhs)

toFloat :: ConstResult -> ConstResult
toFloat x@(FloatResult _) = x
toFloat   (IntResult   i) = FloatResult . fromIntegral $ i

applyBin :: Position 
	 -> CBinaryOp 
	 -> ConstResult 
	 -> ConstResult 
	 -> GB ConstResult
applyBin cpos CMulOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x * y)
applyBin cpos CMulOp (FloatResult x) 
		     (FloatResult y) = return $ FloatResult (x * y)
applyBin cpos CDivOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x `div` y)
applyBin cpos CDivOp (FloatResult x) 
		     (FloatResult y) = return $ FloatResult (x / y)
applyBin cpos CRmdOp (IntResult   x) 
		     (IntResult   y) = return$ IntResult (x `mod` y)
applyBin cpos CRmdOp (FloatResult x) 
		     (FloatResult y) = 
  illegalConstExprErr cpos "a % operator applied to a float"
applyBin cpos CAddOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x + y)
applyBin cpos CAddOp (FloatResult x) 
		     (FloatResult y) = return $ FloatResult (x + y)
applyBin cpos CSubOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x - y)
applyBin cpos CSubOp (FloatResult x) 
		     (FloatResult y) = return $ FloatResult (x - y)
applyBin cpos CShlOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x * 2^y)
applyBin cpos CShlOp (FloatResult x) 
		     (FloatResult y) = 
  illegalConstExprErr cpos "a << operator applied to a float"
applyBin cpos CShrOp (IntResult   x) 
		     (IntResult   y) = return $ IntResult (x `div` 2^y)
applyBin cpos CShrOp (FloatResult x) 
		     (FloatResult y) = 
  illegalConstExprErr cpos "a >> operator applied to a float"
applyBin cpos _      (IntResult   x) 
		     (IntResult   y) = 
  todo "GenBind.applyBin: Not yet implemented operator in constant expression."
applyBin cpos _      (FloatResult x) 
		     (FloatResult y) = 
  todo "GenBind.applyBin: Not yet implemented operator in constant expression."
applyBin _    _      _ _             = 
  interr "GenBind.applyBinOp: Illegal combination!"

applyUnary :: Position -> CUnaryOp -> ConstResult -> GB ConstResult
applyUnary cpos CPreIncOp  _               = 
  illegalConstExprErr cpos "a ++ operator"
applyUnary cpos CPreDecOp  _               = 
  illegalConstExprErr cpos "a -- operator"
applyUnary cpos CPostIncOp _               = 
  illegalConstExprErr cpos "a ++ operator"
applyUnary cpos CPostDecOp _               = 
  illegalConstExprErr cpos "a -- operator"
applyUnary cpos CAdrOp     _               = 
  illegalConstExprErr cpos "a & operator"
applyUnary cpos CIndOp     _               = 
  illegalConstExprErr cpos "a * operator"
applyUnary cpos CPlusOp    arg             = return arg
applyUnary cpos CMinOp     (IntResult   x) = return (IntResult (-x))
applyUnary cpos CMinOp     (FloatResult x) = return (FloatResult (-x))
applyUnary cpos CCompOp    _		   = 
  todo "GenBind.applyUnary: ~ not yet implemented."
applyUnary cpos CNegOp     (IntResult   x) = 
  let r = toInteger . fromEnum $ (x == 0)
  in return (IntResult r)
applyUnary cpos CNegOp     (FloatResult _) = 
  illegalConstExprErr cpos "! applied to a float"


-- error messages
-- --------------

unknownFieldErr          :: Position -> Ident -> GB a
unknownFieldErr cpos ide  =
  raiseErrorCTExc (posOf ide) 
    ["Unknown member name!",
     "The structure has no member called `" ++ identToLexeme ide 
     ++ "'.  The structure is defined at",
     show cpos ++ "."]

illegalStructUnionErr          :: Position -> Position -> GB a
illegalStructUnionErr cpos pos  =
  raiseErrorCTExc pos 
    ["Illegal structure or union type!",
     "There is not automatic support for marshaling of structures and",
     "unions; the offending type is declared at "
     ++ show cpos ++ "."]

illegalTypeSpecErr      :: Position -> GB a
illegalTypeSpecErr cpos  =
  raiseErrorCTExc cpos 
    ["Illegal type!",
     "The type specifiers of this declaration do not form a legal ANSI C(89) \
     \type."
    ]

unsupportedTypeSpecErr      :: Position -> GB a
unsupportedTypeSpecErr cpos  =
  raiseErrorCTExc cpos 
    ["Unsupported type!",
     "The type specifier of this declaration is not supported by your C \
     \compiler."
    ]

variadicErr          :: Position -> Position -> GB a
variadicErr pos cpos  =
  raiseErrorCTExc pos 
    ["Variadic function!",
     "Calling variadic functions is not supported by the FFI; the function",
     "is defined at " ++ show cpos ++ "."]

incompatibleCallHooksErr            :: Position -> Position -> GB a
incompatibleCallHooksErr here there  =
  raiseErrorCTExc here 
    ["Incompatible call hooks!",
     "There is a another call hook for the same C function at " ++ show there,
     "The flags and C function name of the two hooks should be identical,",
     "but they are not."]

illegalConstExprErr           :: Position -> String -> GB a
illegalConstExprErr cpos hint  =
  raiseErrorCTExc cpos ["Illegal constant expression!",
		        "Encountered " ++ hint ++ " in a constant expression,",
		        "which ANSI C89 does not permit."]

voidFieldErr      :: Position -> GB a
voidFieldErr cpos  =
  raiseErrorCTExc cpos ["Void field in struct!",
		        "Attempt to access a structure field of type void."]

structExpectedErr     :: Ident -> GB a
structExpectedErr ide  =
  raiseErrorCTExc (posOf ide) 
    ["Expected a structure or union!",
     "Attempt to access member `" ++ identToLexeme ide ++ "' in something not",
     "a structure or union."]

ptrExpectedErr     :: Position -> GB a
ptrExpectedErr pos  =
  raiseErrorCTExc pos
    ["Expected a pointer!",
     "Attempt to dereference a non-pointer object."]
