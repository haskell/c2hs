--  C->Haskell Compiler: binding generator
--
--  Author : Manuel M. T. Chakravarty
--  Created: 17 August 99
--
--  Version $Revision: 1.24 $ from $Date: 2001/05/03 13:31:41 $
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
--  Pointer map
--  -----------
--
--  Pointer hooks allow the use to customise the Haskell types to which C
--  pointer types are mapped.  The globally maintained map essentially maps C
--  pointer types to Haskell pointer types.  The representation of the Haskell
--  types is defined by the `type' or `newtype' declaration emitted by the
--  corresponding pointer hook.  However, the map stores a flag that tells
--  whether the C type is itself the pointer type in question or whether it is
--  pointers to this C type that should be mapped as specified.
--
--- TODO ----------------------------------------------------------------------
--
--  * A function prototype that uses a defined type on its left hand side may
--    declare a function, while that is not obvious from the declaration
--    itself (without also considering the `typedef').  Calls to such
--    functions are currently rejected, which is a BUG.
--
--  * context hook must preceded all but the import hooks
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
import List       (find, deleteBy, intersperse)
import Maybe	  (catMaybes, isNothing, isJust, fromJust, fromMaybe, 
		   listToMaybe, mapMaybe)
import Monad	  (when, unless, liftM, mapAndUnzipM)
import Array	  (Array, (!))

import Common     (Position, Pos(posOf), nopos)
import Utils	  (lookupBy, mapMaybeM)
import Errors	  (interr, todo)
import Idents     (Ident, identToLexeme, onlyPosIdent, internalIdent)
import Attributes (newAttrsOnlyPos)
import FiniteMaps (FiniteMap, zeroFM, addToFM, lookupFM, joinFM, toListFM,
		   listToFM)

import C2HSConfig (dlsuffix)
import C2HSState  (CST, readCST, transCST, runCST, nop,
		   raiseError, errorsPresent, showErrors, fatal,
		   SwitchBoard(..), Traces(..), putTraceStr,
		   putStrCIO, getSwitch)
import C	  (AttrC, CObj(..), CTag(..), lookupDefObjC, lookupDefTagC,
		   CHeader(..), CExtDecl, CDecl(..), CDeclSpec(..),
		   CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
		   CStructUnion(..), CStructTag(..), CEnum(..), CDeclr(..),
		   CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..),
		   CUnaryOp(..), CConst (..),
		   CT, readCT, transCT, getCHeaderCT, runCT, ifCTExc,
		   raiseErrorCTExc, findFunObj, findTag, findTypeObj,
		   applyPrefixToNameSpaces, isTypedef, simplifyDecl,
		   declrFromDecl, declrNamed, structMembers, structName,
		   declaredName , structFromDecl, funResultAndArgs, 
		   chaseDecl, findAndChaseDecl, checkForAlias, lookupEnum,
		   lookupStructUnion, isPtrDeclr, dropPtrDeclr, isPtrDecl,
		   isFunDeclr)  
import CHS	  (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..),
		   CHSAccess(..), CHSAPath(..),CHSPtrType(..))
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

-- map that for maps C pointer types to Haskell types for pointer that have
-- been registered using a pointer hook
--
-- * the `Bool' indicates whether for a C type "ctype", we map "ctype" itself
--   or "*ctype"
--
-- * in the co-domain, the first string is the type for function arguments and
--   the second string is for function results; this distinction is necessary
--   as `ForeignPtr's cannot be returned by a foreign function; the
--   restriction on function result types is only for the actual result, not
--   for type arguments to parametrised pointer types, ie, it holds for `res'
--   in `Int -> IO res', but not in `Int -> Ptr res'
--
type PointerMap = FiniteMap (Bool, Ident) (String, String)

-- the local state consists of
--
-- (1) the dynamic library specified by the context hook,
-- (2) the prefix specified by the context hook,
-- (3) the set of delayed code fragaments, ie, pieces of Haskell code that,
--     finally, have to be appended at the CHS module together with the hook
--     that created them (the latter allows avoid duplication of foreign
--     export declarations), and
-- (4) a map associating C pointer types with their Haskell representation
--     
-- access to the attributes of the C structure tree is via the `CT' monad of
-- which we use an instance here
--
data GBState  = GBState {
		  lib     :: String,		   -- dynamic library
		  prefix  :: String,		   -- prefix
	          frags   :: [(CHSHook, CHSFrag)], -- delayed code (with hooks)
		  ptrmap  :: PointerMap		   -- pointer representation
	       }

type GB a = CT GBState a

initialGBState :: GBState
initialGBState  = GBState {
		    lib    = "",
		    prefix = "",
		    frags  = [],
		    ptrmap = zeroFM
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

-- add an entry to the pointer map
--
ptrMapsTo :: (Bool, Ident) -> (String, String) -> GB ()
(isStar, cName) `ptrMapsTo` hsRepr =
  transCT (\state -> (state { 
		        ptrmap = addToFM (isStar, cName) hsRepr (ptrmap state)
		      }, ()))

-- query the pointer map
--
queryPtr        :: (Bool, Ident) -> GB (Maybe (String, String))
queryPtr pcName  = do
		     fm <- readCT ptrmap
		     return $ lookupFM fm pcName

-- merge a pointer map
--
-- FIXME: This currently has several shortcomings:
--	  * It just dies in case of a corrupted .chi file
--	  * We probably would like to raise an error if there are colliding
--	    entries during the `joinFM'
--	  * Do we want position information associated with the read idents?
--
mergePtrMap     :: String -> GB ()
mergePtrMap str  =
  transCT (\state -> (state { 
		        ptrmap = joinFM (ptrmap state) readPtrMap
		      }, ()))
  where
    readPtrMap = listToFM [((isStar, internalIdent ide), repr)
			  | ((isStar, ide), repr) <- read str]

-- convert the whole pointer map into printable form
--
dumpPtrMap :: GB String
dumpPtrMap  = do
	        fm <- readCT ptrmap
		return $ show [((isStar, identToLexeme ide), repr)
			      | ((isStar, ide), repr) <- toListFM fm]


-- expansion of the hooks
-- ----------------------

-- given a C header file and a binding file, expand all hooks in the binding
-- file using the C header information (EXPORTED)
--
-- * together with the module, returns the contents of the .chi file
--
-- * if any error (not warnings) is encountered, a fatal error is raised.
--
-- * also returns all warning messages encountered (last component of result)
--
expandHooks        :: AttrC -> CHSModule -> CST s (CHSModule, String, String)
expandHooks ac mod  = do
		        (_, res) <- runCT (expandModule mod) ac initialGBState
			return res

expandModule		       :: CHSModule -> GB (CHSModule, String, String)
expandModule (CHSModule frags)  =
  do
    -- expand hooks
    --
    traceInfoExpand
    frags'       <- mapM expandFrag frags
    delayedFrags <- getDelayedCode

    -- get .chi dump
    --
    chi <- dumpPtrMap

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
	return (CHSModule (frags' ++ delayedFrags), chi, warnmsgs)
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
expandHook (CHSImport qual ide chi _) =
  do
    mergePtrMap chi
    return $ 
      "import " ++ (if qual then "qualified " else "") ++ identToLexeme ide
expandHook (CHSContext _ olib oprefix _) =
  do
    setContext olib oprefix		      -- enter context information
    mapMaybeM applyPrefixToNameSpaces oprefix -- use the prefix on name spaces
    return ""
expandHook (CHSType ide pos) =
  do
    traceInfoType
    decl <- findAndChaseDecl ide False True	-- no indirection, but shadows
    ty <- extractSimpleType False pos decl
    traceInfoDump decl ty
    return $ showExtType ty
  where
    traceInfoType         = putTraceStr traceGenBindSW "** Type hook:\n"
    traceInfoDump decl ty = putTraceStr traceGenBindSW $
      "Declaration\n<need ppr for cdecl>\ntranslates to\n" 
      ++ showExtType ty ++ "\n"
expandHook (CHSEnum cide oalias chsTrans derive _) =
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
    enumDef cide enum hide trans (map identToLexeme derive)
expandHook hook@(CHSCall isFun isUns ide oalias pos) =
  do
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used 
    -- afterwards instead of the original one
    --
    (ObjCO cdecl, ide) <- findFunObj ide True
    let ideLexeme = identToLexeme ide  -- orignal name might have been a shadow
	hsLexeme  = ideLexeme `maybe` identToLexeme $ oalias
        cdecl'    = ide `simplifyDecl` cdecl
    --
    -- compute the external type from the declaration, get the library, and
    -- delay the foreign export declaration
    --
    extType <- extractFunType pos cdecl' isFun
    lib     <- getLibrary
    delayCode hook (foreignImport lib ideLexeme hsLexeme isUns extType)
    return hsLexeme
expandHook (CHSField access path pos) =
  do
    (decl, offsets) <- accessPath path
    ty <- extractSimpleType False pos decl
    setGet pos access offsets ty
expandHook (CHSPointer isStar cName oalias ptrKind isNewtype oRefType pos) =
  do
    -- look up the original name in the C syntax tree using shadow entries;
    -- if the user does not supply her own Haskell name we generate one from
    -- the C name
    --
    (_, cNameFull) <- findTypeObj cName True
    unless isStar $ 
      assertIsPtr cNameFull
    let hsName = identToLexeme $ fromMaybe cName oalias
    hsType <- case oRefType of
		Nothing     -> do
			         cDecl <- chaseDecl cNameFull (not isStar)
				 et    <- extractPtrType cDecl
				 return $ showExtType et
		Just hsType -> return (identToLexeme hsType)
    keepOld <- getSwitch oldFFI
    let ptrArg  = if keepOld 
		  then "()"		-- legacy FFI interface
		  else if isNewtype 
		  then hsName		-- abstract type
		  else hsType		-- concrete type
	ptrType = show ptrKind ++ " (" ++ ptrArg ++ ")"
	thePtr  = (isStar, cNameFull)
    case ptrKind of
      CHSForeignPtr -> thePtr `ptrMapsTo` (hsName, "Ptr (" ++ ptrArg ++ ")")
      _		    -> thePtr `ptrMapsTo` (hsName, hsName)
    return $
      if isNewtype 
      then "newtype " ++ hsName ++ " = " ++ hsName ++ "(" ++ ptrType ++ ")"
      else "type "    ++ hsName ++ " = "                  ++ ptrType
  where
    assertIsPtr ide = do
		        isPtr <- isPtrDecl ide
			unless isPtr $
			  ptrExpectedErr (posOf cName)

-- produce code for an enumeration
--
-- * an extra instance declaration is required when any of the enumeration
--   constants is explicitly assigned a value in its definition
--
-- * the translation function strips prefixes where possible (different
--   enumerators maye have different prefixes)
--
enumDef :: Ident -> CEnum -> String -> TransFun -> [String] -> GB String
enumDef _ cenum@(CEnum _ list _) hident trans userDerive =
  do
    (list', enumAuto) <- evalTagVals list
    let enumVals = [(trans ide, cexpr) | (ide, cexpr) <-  list']  -- translate
        defHead  = enumHead hident
	defBody  = enumBody (length defHead - 2) enumVals
	inst	 = makeDerives 
		   (if enumAuto then "Enum" : userDerive else userDerive) ++
		   if enumAuto then "\n" else "\n" ++ enumInst hident enumVals
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
    makeDerives [] = ""
    makeDerives dList = "deriving (" ++ concat (intersperse "," dList) ++")"

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
			  CHSSet -> "(\\ptr val -> do {"
			  CHSGet -> "(\\ptr -> do {"
    body <- setGetBody (reverse offsets)
    return $ pre ++ body ++ "})"
  where
    setGetBody [offset] =
      do
	let typeTag = showExtType' NameBL ty
        checkType ty
	return $ case access of
	  CHSSet -> "pokeByteOff ptr " ++ show offset 
		    ++ " (val::" ++ typeTag ++ ")"
	  CHSGet -> "peekByteOff ptr " ++ show offset ++ " ::IO " ++ typeTag
      where
        -- check that the type can be marshalled
	--
	checkType (FunET     at rt) = checkType at >> checkType rt
	checkType (IOET      _    ) = interr "GenBind.setGet: Illegal type!"
	checkType (PtrET     t    ) = checkType t
	checkType (DefinedET _ _  ) = return ()  -- can't check any further
	checkType (PrimET    _    ) = return ()
	checkType (UnitET         ) = voidFieldErr pos
    setGetBody (offset:offsets) =
      do
	code <- setGetBody offsets
	return $ "ptr <- peekByteOff ptr " ++ show offset ++ code
--	return $ "ptr <- peekByteOff ptr " ++ show offset ++ "::IO Ptr (Ptr ?);" ++ code
-- should be sufficient without explicit type


-- C code computations
-- -------------------

-- the result of a constant expression
--
data ConstResult = IntResult   Integer
		 | FloatResult Float

-- types that may occur in foreign declarations, ie, Haskell land types
--
-- * we reprsent C functions with no arguments (ie, the ANSI C `void'
--   argument) by `FunET UnitET res' rather than just `res' internally,
--   although the latter representation is finally emitted into the binding
--   file; this is because we need to know which types are functions (in
--   particular, to distinguish between `Ptr a' and `FunPtr a')
--
-- * aliased types (`DefinedET') are represented by a string plus their C
--   declaration; the latter is for functions interpreting the following
--   structure 
--
-- * the representation for pointers does not distinguish between normal,
--   function, foreign, and stable pointers; function pointers are identified
--   by their argument and foreign and stable pointers are only used
--   indirectly, by referring to type names introduced by a `pointer' hook
--
data ExtType = FunET     ExtType ExtType	-- function
	     | IOET      ExtType		-- operation with side effect
	     | PtrET	 ExtType	        -- typed pointer
	     | DefinedET CDecl String		-- aliased type
	     | PrimET    CPrimType		-- basic C type
	     | UnitET				-- void

-- composite C type
--
data CompType = ExtType  ExtType		-- external type
	      | SUType	 CStructUnion		-- structure or union

-- pretty print an external type
--
showExtType :: ExtType -> String
showExtType  = showExtType' NoBL

-- to keep track of the brace environment
--
data BraceLevel = NoBL
	        | FunBL
	        | NameBL	
		deriving (Eq, Ord)

-- conditionally add parenthesis to a string
--
maybeParen :: BraceLevel        -- current brace level
	   -> BraceLevel	-- minimum brace level that requires parens
	   -> String		-- the maybe parenthesised string
	   -> String
maybeParen currBL minBL str | currBL >= minBL = "(" ++ str ++ ")"
			    | otherwise       = str

-- pretty print an external type with a minimum of braces
--
showExtType' :: BraceLevel -> ExtType -> String
showExtType' b (FunET UnitET res)  = showExtType' b res
showExtType' b (FunET arg res)	   = maybeParen b FunBL $ 
				       showExtType' FunBL arg ++ " -> " 
				       ++ showExtType' NoBL res
showExtType' b (IOET t)		   = maybeParen b NameBL $ 
				       "IO " ++ showExtType' NameBL t
showExtType' b (PtrET t)	   = let ptrCon = case t of
						    FunET _ _ -> "FunPtr"
						    _	      -> "Ptr"
				     in
				     maybeParen b NameBL $ 
				       ptrCon ++ " " ++ showExtType' NameBL t
showExtType' _ (DefinedET _ str)     = str
showExtType' _ (PrimET CPtrPT)     = "Ptr ()"
showExtType' _ (PrimET CFunPtrPT)  = "FunPtr ()"
showExtType' _ (PrimET CCharPT)    = "CChar"
showExtType' _ (PrimET CUCharPT)   = "CUChar"
showExtType' _ (PrimET CSCharPT)   = "CSChar"
showExtType' _ (PrimET CIntPT)     = "CInt"
showExtType' _ (PrimET CShortPT)   = "CShort"
showExtType' _ (PrimET CLongPT)    = "CLong"
showExtType' _ (PrimET CLLongPT)   = "CLLong"
showExtType' _ (PrimET CUIntPT)    = "CUInt"
showExtType' _ (PrimET CUShortPT)  = "CUShort"
showExtType' _ (PrimET CULongPT)   = "CULong"
showExtType' _ (PrimET CULLongPT)  = "CULLong"
showExtType' _ (PrimET CFloatPT)   = "CFloat"
showExtType' _ (PrimET CDoublePT)  = "CDouble"
showExtType' _ (PrimET CLDoublePT) = "CLDouble"
showExtType' _ UnitET		   = "()"

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
extractFunType                  :: Position -> CDecl -> Bool -> GB ExtType
extractFunType pos cdecl isPure  = 
  do
    -- remove all declarators except that of the function we are processing;
    -- then, extract the functions arguments and result type (also check that
    -- the function is not variadic); finally, compute the external type for
    -- the result
    --
    let (args, resultDecl, variadic) = funResultAndArgs cdecl
    when variadic $
      variadicErr pos cpos
    preResultType <- extractSimpleType True pos resultDecl
    --
    -- we can now add the `IO' monad if this is no pure function 
    --
    let resultType = if isPure 
		     then      preResultType 
		     else IOET preResultType
    --
    -- compute function arguments and create a function type (a function
    -- prototype with `void' as its single argument declares a nullary
    -- function) 
    --
    argTypes <- mapM (extractSimpleType False pos) args
    return $ foldr FunET resultType argTypes
  where
    cpos = posOf cdecl

-- compute a non-struct/union type from the given declaration 
--
-- * the declaration may have at most one declarator
--
-- * C functions are represented as `Ptr (FunEt ...)' or `Addr' if in
--   compatibility mode (ie, `--old-ffi=yes')
--
extractSimpleType                    :: Bool -> Position -> CDecl -> GB ExtType
extractSimpleType isResult pos cdecl  =
  do
    ct <- extractCompType isResult cdecl
    case ct of
      ExtType et -> return et
      SUType  _  -> illegalStructUnionErr (posOf cdecl) pos

-- compute a Haskell type for a type referenced in a C pointer type
--
-- * the declaration may have at most one declarator
--
-- * struct/union types are mapped to `()'
--
-- * NB: this is by definition not a result type
--
extractPtrType       :: CDecl -> GB ExtType
extractPtrType cdecl  = do
  ct <- extractCompType False cdecl
  case ct of
    ExtType et -> return et
    SUType  _  -> return UnitET

-- compute a Haskell type from the given C declaration, where C functions are
-- represented by function pointers
--
-- * the declaration may have at most one declarator
--
-- * all C pointers (including functions) are represented as `Addr' if in
--   compatibility mode (--old-ffi)
--
-- * typedef'ed types are chased
--
-- * the first argument specifies whether the type specifies the result of a
--   function (these is only applicable to direct results and not to type
--   parameters for pointers that are a result)
--
extractCompType :: Bool -> CDecl -> GB CompType
extractCompType isResult cdecl@(CDecl specs declrs ats)  = 
  case declrs of
    []                                      -> aliasOrSpecType
    [(Just declr, _, _)] | isPtrDeclr declr -> ptrType declr
			 | isFunDeclr declr -> funType
			 | otherwise	    -> aliasOrSpecTypeDeclr
    _                                       ->
      interr "GenBind.extractCompType: Too many declarators!"
  where
    -- handle explicit pointer types
    --
    ptrType declr = do
      oHsRepr <- case declaredName cdecl of  -- check for pointer hook alias
		   Nothing  -> return Nothing
		   Just ide -> queryPtr (True, ide)
      case oHsRepr of
	Nothing             -> do
	  let declrs' = dropPtrDeclr declr
	      cdecl'  = CDecl specs [(Just declrs', Nothing, Nothing)] ats
	  ct <- extractCompType False cdecl'
	  returnX $ case ct of
		      ExtType et -> PtrET et
		      SUType  _  -> PtrET UnitET
        Just (repr1, repr2) -> 
	  returnX $ DefinedET cdecl (if isResult then repr2 else repr1)
    --
    -- handle explicit function types
    --
    -- FIXME: we currently regard any functions as being impure (ie, being IO
    --	      functions); is this ever going to be a problem?
    --
    funType = do
	        et <- extractFunType (posOf cdecl) cdecl False
		returnX et
    --
    -- if the declarator is not anonymous, check for a pointer hook without an
    -- explicit `*' 
    --
    aliasOrSpecTypeDeclr =
      case declaredName cdecl of
        Nothing  -> aliasOrSpecType
	Just ide -> do
		      oHsRepr <- queryPtr (False, ide)
		      case oHsRepr of
			Nothing             -> aliasOrSpecType
		        Just (repr1, repr2) -> 
			  returnX $ 
			    DefinedET cdecl (if isResult then repr2 else repr1)
    --
    -- handle all types, which are not obviously pointers or functions 
    --
    aliasOrSpecType = do
		        oalias <- checkForAlias cdecl
			case oalias of
			  Nothing    -> specType (posOf cdecl) specs
			  Just decl' -> extractCompType isResult decl'
    --
    -- wrap an `ExtType' into a `CompType' and convert parametrised pointers
    -- to `Addr' if needed
    --
    returnX retval@(PtrET et) = do
				  keepOld <- getSwitch oldFFI
				  if keepOld 
				    then return $ ExtType (PrimET CPtrPT)
				    else return $ ExtType retval
    returnX retval            = return $ ExtType retval

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
-- * may not be called for a specifier that defines a typedef alias
--
specType            :: Position -> [CDeclSpec] -> GB CompType
specType cpos specs  = 
  let tspecs = [ts | CTypeSpec ts <- specs]
  in case lookupTSpec tspecs typeMap of
    Just et -> if isUnsupportedType et
	         then unsupportedTypeSpecErr cpos
	         else return $ ExtType et
    Nothing -> case tspecs of
	         [CSUType   cu _] -> return $ SUType cu     -- struct or union
		 [CEnumType _  _] -> return $ ExtType (PrimET CIntPT)  -- enum
	         [CTypeDef  _  _] -> 
		   interr "GenBind.specType: Illegal typedef alias!"
		 _		  -> illegalTypeSpecErr cpos
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
    ct <- extractCompType False cdecl
    case ct of
      ExtType (FunET _ _        ) -> return (sizes!CFunPtrPT, 
					     alignments!CFunPtrPT)
      ExtType (IOET  _          ) -> interr "GenBind.sizeof: Illegal IO type!"
      ExtType (PtrET (FunET _ _)) -> return (sizes!CFunPtrPT, 
					     alignments!CFunPtrPT)
      ExtType (PtrET _          ) -> return (sizes!CPtrPT, alignments!CPtrPT)
      ExtType (DefinedET decl _ ) -> sizeAlignOf decl
      ExtType (PrimET pt        ) -> return (sizes!pt, alignments!pt)
      ExtType UnitET              -> voidFieldErr (posOf cdecl)
      SUType su                   -> 
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
    ["Expected a pointer object!",
     "Attempt to dereference a non-pointer object or to use it in a `pointer' \
     \hook."]
