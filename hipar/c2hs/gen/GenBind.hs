--  C->Haskell Compiler: binding generator
--
--  Author : Manuel M T Chakravarty
--  Created: 17 August 99
--
--  Version $Revision: 1.51 $ from $Date: 2004/06/11 07:10:17 $
--
--  Copyright (c) [1999..2003] Manuel M T Chakravarty
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
--  traced back to their original definitions.  Pointer types are mapped
--  to `Ptr a' or `FunPtr a' depending on whether they point to a functional.
--  Values obtained from bit fields are represented by `CInt' or `CUInt'
--  depending on whether they are signed.
--
--  We obtain the size and alignment constraints for all primitive types of C
--  from `CInfo', which obtains it from the Haskell 98 FFI.  In the alignment
--  computations involving bit fields, we assume that the alignment
--  constraints for bitfields (wrt to non-bitfield members) is always the same
--  as for `int' irrespective of the size of the bitfield.  This seems to be
--  implicitly guaranteed by K&R A8.3, but it is not entirely clear.
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
--  We generally use `shadow' lookups.  When an identifier cannot be found,
--  we check whether - according to the prefix set by the context hook -
--  another identifier casts a shadow that matches.  If so, that identifier is
--  taken instead of the original one.
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
--  * The use of `++' in the recursive definition of the routines generating
--    `Enum' instances is not particularly efficient.
--
--  * Some operands are missing in `applyBin' - unfortunately, Haskell does
--    not have standard bit operations.   Some constructs are also missing
--    from `evalConstCExpr'.  Haskell 98 FFI standardises `Bits'; use that.
--

module GenBind (expandHooks) 
where 

-- standard libraries
import Char	  (toUpper, toLower, isSpace)
import List       (deleteBy, intersperse, isPrefixOf)
import Maybe	  (isNothing, isJust, fromJust, fromMaybe)
import Monad	  (when, unless, liftM, mapAndUnzipM)

-- Compiler Toolkit
import Common     (Position, Pos(posOf), nopos, builtinPos)
import Utils	  (lookupBy, mapMaybeM)
import Errors	  (interr, todo)
import Idents     (Ident, identToLexeme, onlyPosIdent)
import Attributes (newAttrsOnlyPos)

-- C->Haskell
import C2HSConfig (dlsuffix)
import C2HSState  (CST, nop, errorsPresent, showErrors, fatal,
		   SwitchBoard(..), Traces(..), putTraceStr, getSwitch,
		   printCIO)
import C	  (AttrC, CObj(..), CTag(..), lookupDefObjC, lookupDefTagC,
		   CHeader(..), CExtDecl, CDecl(..), CDeclSpec(..),
		   CStorageSpec(..), CTypeSpec(..), CTypeQual(..),
		   CStructUnion(..), CStructTag(..), CEnum(..), CDeclr(..),
		   CInit(..), CExpr(..), CAssignOp(..), CBinaryOp(..),
		   CUnaryOp(..), CConst (..),
		   CT, readCT, transCT, getCHeaderCT, runCT, ifCTExc,
		   raiseErrorCTExc, findValueObj, findFunObj, findTag,
		   findTypeObj, applyPrefixToNameSpaces, isTypedef,
		   simplifyDecl, declrFromDecl, declrNamed, structMembers,
		   structName, tagName, declaredName , structFromDecl,
		   funResultAndArgs, chaseDecl, findAndChaseDecl,
		   checkForAlias, checkForOneAliasName, lookupEnum,
		   lookupStructUnion, lookupDeclOrTag, isPtrDeclr,
		   dropPtrDeclr, isPtrDecl, getDeclOf, isFunDeclr,
		   refersToNewDef, CDef(..))

-- friends
import CHS	  (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..),
		   CHSParm(..), CHSArg(..), CHSAccess(..), CHSAPath(..),
		   CHSPtrType(..), showCHSParm) 
import CInfo      (CPrimType(..), size, alignment, bitfieldIntSigned,
		   bitfieldAlignment)
import GBMonad    (TransFun, transTabToTransFun, HsObject(..), GB,
		   initialGBState, setContext, getPrefix, 
		   delayCode, getDelayedCode, ptrMapsTo, queryPtr, objIs,
		   queryObj, queryClass, queryPointer, mergeMaps, dumpMaps)


-- default marshallers
-- -------------------

-- FIXME: 
-- - we might have a dynamically extended table in the monad if needed (we
--   could marshall enums this way)
-- - the checks for the Haskell types are quite kludgy

-- determine the default "in" marshaller for the given Haskell and C types
--
lookupDftMarshIn :: String -> [ExtType] -> GB (Maybe (Ident, CHSArg))
lookupDftMarshIn "Bool"   [PrimET pt] | isIntegralCPrimType pt = 
  return $ Just (cFromBoolIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isIntegralHsType hsTy 
				      &&isIntegralCPrimType pt = 
  return $ Just (cIntConvIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isFloatHsType hsTy 
				      &&isFloatCPrimType pt    = 
  return $ Just (cFloatConvIde, CHSValArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT)]             =
  return $ Just (withCStringIde, CHSIOArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT), PrimET pt]  
  | isIntegralCPrimType pt				       =
  return $ Just (withCStringLenIde, CHSIOArg)
lookupDftMarshIn hsTy     [PtrET ty]  | showExtType ty == hsTy =
  return $ Just (withIde, CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]  
  | isIntegralHsType hsTy && isIntegralCPrimType pt            =
  return $ Just (withIntConvIde, CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]  
  | isFloatHsType hsTy && isFloatCPrimType pt                  =
  return $ Just (withFloatConvIde, CHSIOArg)
lookupDftMarshIn "Bool"   [PtrET (PrimET pt)]  
  | isIntegralCPrimType pt                                     =
  return $ Just (withFromBoolIde, CHSIOArg)
-- FIXME: handle array-list conversion
lookupDftMarshIn _        _                                    = 
  return Nothing

-- determine the default "out" marshaller for the given Haskell and C types
--
lookupDftMarshOut :: String -> [ExtType] -> GB (Maybe (Ident, CHSArg))
lookupDftMarshOut "()"     _                                    =
  return $ Just (voidIde, CHSVoidArg)
lookupDftMarshOut "Bool"   [PrimET pt] | isIntegralCPrimType pt = 
  return $ Just (cToBoolIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isIntegralHsType hsTy 
				       &&isIntegralCPrimType pt = 
  return $ Just (cIntConvIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isFloatHsType hsTy 
				       &&isFloatCPrimType pt    = 
  return $ Just (cFloatConvIde, CHSValArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT)]             =
  return $ Just (peekCStringIde, CHSIOArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT), PrimET pt]  
  | isIntegralCPrimType pt				        =
  return $ Just (peekCStringLenIde, CHSIOArg)
lookupDftMarshOut hsTy     [PtrET ty]  | showExtType ty == hsTy =
  return $ Just (peekIde, CHSIOArg)
-- FIXME: add combination, such as "peek" plus "cIntConv" etc
-- FIXME: handle array-list conversion
lookupDftMarshOut _        _                                    = 
  return Nothing


-- check for integral Haskell types
--
isIntegralHsType :: String -> Bool
isIntegralHsType "Int"    = True
isIntegralHsType "Int8"   = True
isIntegralHsType "Int16"  = True
isIntegralHsType "Int32"  = True
isIntegralHsType "Int64"  = True
isIntegralHsType "Word8"  = True
isIntegralHsType "Word16" = True
isIntegralHsType "Word32" = True
isIntegralHsType "Word64" = True
isIntegralHsType _	  = False

-- check for floating Haskell types
--
isFloatHsType :: String -> Bool
isFloatHsType "Float"  = True
isFloatHsType "Double" = True
isFloatHsType _	       = False

-- check for integral C types
--
isIntegralCPrimType :: CPrimType -> Bool
isIntegralCPrimType  = (`elem` [CIntPT, CShortPT, CLongPT, CLLongPT, CUIntPT, 
			        CUShortPT, CULongPT, CULLongPT])

-- check for floating C types
--
isFloatCPrimType :: CPrimType -> Bool
isFloatCPrimType  = (`elem` [CFloatPT, CDoublePT, CLDoublePT])

-- standard conversions
--
voidIde           = noPosIdent "void"	      -- never appears in the output
cFromBoolIde      = noPosIdent "cFromBool"
cToBoolIde        = noPosIdent "cToBool"
cIntConvIde       = noPosIdent "cIntConv"
cFloatConvIde     = noPosIdent "cFloatConv"
withIde           = noPosIdent "withObject"   -- FIXME: should be "with"
withCStringIde    = noPosIdent "withCString"
withCStringLenIde = noPosIdent "withCStringLenIntConv"
withIntConvIde    = noPosIdent "withIntConv"
withFloatConvIde  = noPosIdent "withFloatConv"
withFromBoolIde   = noPosIdent "withFromBoolConv"
peekIde           = noPosIdent "peek"
peekCStringIde    = noPosIdent "peekCString"
peekCStringLenIde = noPosIdent "peekCStringLenIntConv"


-- expansion of binding hooks
-- --------------------------

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
    frags'       <- expandFrags frags
    delayedFrags <- getDelayedCode

    -- get .chi dump
    --
    chi <- dumpMaps

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

expandFrags :: [CHSFrag] -> GB [CHSFrag]
expandFrags = liftM concat . mapM expandFrag

expandFrag :: CHSFrag -> GB [CHSFrag]
expandFrag verb@(CHSVerb _ _     ) = return [verb]
expandFrag      (CHSHook h       ) = 
  do
    code <- expandHook h
    return [CHSVerb code builtinPos]
  `ifCTExc` return [CHSVerb "** ERROR **" builtinPos]
expandFrag      (CHSCPP  s _     ) = 
  interr $ "GenBind.expandFrag: Left over CHSCPP!\n---\n" ++ s ++ "\n---"
expandFrag      (CHSC    s _     ) = 
  interr $ "GenBind.expandFrag: Left over CHSC!\n---\n" ++ s ++ "\n---"
expandFrag      (CHSCond alts dft) = 
  do
    traceInfoCond
    select alts
  where
    select []                  = do
				   traceInfoDft dft
				   expandFrags (maybe [] id dft)
    select ((ide, frags):alts) = do
				   oobj <- findTag ide
				   traceInfoVal ide oobj
				   if isNothing oobj
				     then
				       select alts
				     else	     -- found right alternative
				       expandFrags frags
    --
    traceInfoCond         = traceGenBind "** CPP conditional:\n"
    traceInfoVal ide oobj = traceGenBind $ identToLexeme ide ++ " is " ++
			      (if isNothing oobj then "not " else "") ++
			      "defined.\n"
    traceInfoDft dft      = if isNothing dft 
			    then 
			      return () 
			    else 
			      traceGenBind "Choosing else branch.\n"

expandHook :: CHSHook -> GB String
expandHook (CHSImport qual ide chi _) =
  do
    mergeMaps chi
    return $ 
      "import " ++ (if qual then "qualified " else "") ++ identToLexeme ide
expandHook (CHSContext olib oprefix _) =
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
    return $ "(" ++ showExtType ty ++ ")"
  where
    traceInfoType         = traceGenBind "** Type hook:\n"
    traceInfoDump decl ty = traceGenBind $
      "Declaration\n" ++ show decl ++ "\ntranslates to\n" 
      ++ showExtType ty ++ "\n"
expandHook (CHSSizeof ide pos) =
  do
    traceInfoSizeof
    decl <- findAndChaseDecl ide False True	-- no indirection, but shadows
    (size, _) <- sizeAlignOf decl
    traceInfoDump decl size
    return $ show (fromIntegral . padBits $ size)
  where
    traceInfoSizeof         = traceGenBind "** Sizeof hook:\n"
    traceInfoDump decl size = traceGenBind $
      "Size of declaration\n" ++ show decl ++ "\nis " 
      ++ show (fromIntegral . padBits $ size) ++ "\n"
expandHook (CHSEnum cide oalias chsTrans oprefix derive _) =
  do
    -- get the corresponding C declaration
    --
    enum <- lookupEnum cide True	-- smart lookup incl error handling
    --
    -- convert the translation table and generate data type definition code
    --
    gprefix <- getPrefix
    let prefix = case oprefix of
		   Nothing -> gprefix
		   Just pref -> pref

    let trans = transTabToTransFun prefix chsTrans
	hide  = identToLexeme . fromMaybe cide $ oalias
    enumDef enum hide trans (map identToLexeme derive)
expandHook hook@(CHSCall isPure isUns ide oalias pos) =
  do
    traceEnter
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used 
    -- afterwards instead of the original one
    --
    (ObjCO cdecl, ide) <- findFunObj ide True
    let ideLexeme = identToLexeme ide  -- orignal name might have been a shadow
	hsLexeme  = ideLexeme `maybe` identToLexeme $ oalias
        cdecl'    = ide `simplifyDecl` cdecl
    callImport hook isPure isUns ideLexeme hsLexeme cdecl' pos
    return hsLexeme
  where
    traceEnter = traceGenBind $ 
      "** Call hook for `" ++ identToLexeme ide ++ "':\n"
expandHook hook@(CHSFun isPure isUns ide oalias ctxt parms parm pos) =
  do
    traceEnter
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used 
    -- afterwards instead of the original one
    --
    (ObjCO cdecl, ide) <- findFunObj ide True
    let ideLexeme = identToLexeme ide  -- orignal name might have been a shadow
	hsLexeme  = ideLexeme `maybe` identToLexeme $ oalias
	fiLexeme  = hsLexeme ++ "'_"   -- *Urgh* - probably unqiue...
	fiIde     = onlyPosIdent nopos fiLexeme
        cdecl'    = ide `simplifyDecl` cdecl
	callHook  = CHSCall isPure isUns ide (Just fiIde) pos
    callImport callHook isPure isUns ideLexeme fiLexeme cdecl' pos
    funDef isPure hsLexeme fiLexeme cdecl' ctxt parms parm pos
  where
    traceEnter = traceGenBind $ 
      "** Fun hook for `" ++ identToLexeme ide ++ "':\n"
expandHook (CHSField access path pos) =
  do
    traceInfoField
    (decl, offsets) <- accessPath path
    traceDepth offsets
    ty <- extractSimpleType False pos decl
    traceValueType ty
    setGet pos access offsets ty
  where
    accessString       = case access of
		           CHSGet -> "Get"
		           CHSSet -> "Set"
    traceInfoField     = traceGenBind $ "** " ++ accessString ++ " hook:\n"
    traceDepth offsets = traceGenBind $ "Depth of access path: " 
					++ show (length offsets) ++ "\n"
    traceValueType et  = traceGenBind $ 
      "Type of accessed value: " ++ showExtType et ++ "\n"
expandHook (CHSPointer isStar cName oalias ptrKind isNewtype oRefType pos) =
  do
    traceInfoPointer
    let hsIde  = fromMaybe cName oalias
	hsName = identToLexeme hsIde
    hsIde `objIs` Pointer ptrKind isNewtype	-- register Haskell object
    --
    -- we check for a typedef declaration or tag (struct, union, or enum)
    --
    declOrTag <- lookupDeclOrTag cName True
    case declOrTag of
      Left cdecl -> do				-- found a typedef declaration
	cNameFull <- case declaredName cdecl of
		       Just ide -> return ide
		       Nothing  -> interr 
				     "GenBind.expandHook: Where is the name?"
	cNameFull `refersToNewDef` ObjCD (TypeCO cdecl) 
				   -- assoc needed for chasing
	traceInfoCName "declaration" cNameFull
	unless (isStar || isPtrDecl cdecl) $ 
	  ptrExpectedErr (posOf cName)
	(hsType, isFun) <- 
	  case oRefType of
	    Nothing     -> do
			     cDecl <- chaseDecl cNameFull (not isStar)
			     et    <- extractPtrType cDecl
			     let et' = adjustPtr isStar et
			     return (showExtType et', isFunExtType et')
	    Just hsType -> return (identToLexeme hsType, False)
	    -- FIXME: it is not possible to determine whether `hsType'
	    --   is a function; we would need to extend the syntax to
	    --   allow `... -> fun HSTYPE' to explicitly mark function
	    --   types if this ever becomes important
	traceInfoHsType hsName hsType
	pointerDef isStar cNameFull hsName ptrKind isNewtype hsType isFun
      Right tag -> do			        -- found a tag definition
        let cNameFull = tagName tag
	traceInfoCName "tag definition" cNameFull
	unless isStar $				-- tags need an explicit `*'
	  ptrExpectedErr (posOf cName)
	let hsType = case oRefType of
		       Nothing     -> "()"
		       Just hsType -> identToLexeme hsType
	traceInfoHsType hsName hsType
	pointerDef isStar cNameFull hsName ptrKind isNewtype hsType False
  where
    -- remove a pointer level if the first argument is `False'
    --
    adjustPtr True  et         = et
    adjustPtr False (PtrET et) = et
    adjustPtr _	    _	       = interr "GenBind.adjustPtr: Where is the Ptr?"
    --
    traceInfoPointer        = traceGenBind "** Pointer hook:\n"
    traceInfoCName kind ide = traceGenBind $ 
      "found C " ++ kind ++ " for `" ++ identToLexeme ide ++ "'\n"
    traceInfoHsType name ty = traceGenBind $ 
      "associated with Haskell entity `" ++ name ++ "'\nhaving type " ++ ty 
      ++ "\n"
expandHook (CHSClass oclassIde classIde typeIde pos) =
  do
    traceInfoClass
    classIde `objIs` Class oclassIde typeIde	-- register Haskell object
    superClasses <- collectClasses oclassIde
    Pointer ptrType isNewtype <- queryPointer typeIde
    when (ptrType == CHSStablePtr) $
      illegalStablePtrErr pos
    classDef pos (identToLexeme classIde) (identToLexeme typeIde) 
	     ptrType isNewtype superClasses
  where
    -- compile a list of all super classes (the direct super class first)
    --
    collectClasses            :: Maybe Ident -> GB [(String, String, HsObject)]
    collectClasses Nothing     = return []
    collectClasses (Just ide)  = 
      do
	Class oclassIde typeIde <- queryClass ide
	ptr			<- queryPointer typeIde
	classes			<- collectClasses oclassIde
	return $ (identToLexeme ide, identToLexeme typeIde, ptr) : classes
    --
    traceInfoClass = traceGenBind $ "** Class hook:\n"

-- produce code for an enumeration
--
-- * an extra instance declaration is required when any of the enumeration
--   constants is explicitly assigned a value in its definition
--
-- * the translation function strips prefixes where possible (different
--   enumerators maye have different prefixes)
--
enumDef :: CEnum -> String -> TransFun -> [String] -> GB String
enumDef cenum@(CEnum _ list _) hident trans userDerive =
  do
    (list', enumAuto) <- evalTagVals list
    let enumVals = [(trans ide, cexpr) | (ide, cexpr) <-  list']  -- translate
        defHead  = enumHead hident
	defBody  = enumBody (length defHead - 2) enumVals
	inst	 = makeDerives 
		   (if enumAuto then "Enum" : userDerive else userDerive) ++
		   if enumAuto then "\n" else "\n" ++ enumInst hident enumVals
    return $ defHead ++ defBody ++ inst
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
    toDef []                _ = 
      "  toEnum unmatched = error (\"" ++ ident 
      ++ ".toEnum: Cannot match \" ++ show unmatched)\n"
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

-- generate a foreign import declaration that is put into the delayed code
--
-- * the C declaration is a simplified declaration of the function that we
--   want to import into Haskell land
--
callImport :: CHSHook -> Bool -> Bool -> String -> String -> CDecl -> Position
	   -> GB ()
callImport hook isPure isUns ideLexeme hsLexeme cdecl pos =
  do
    -- compute the external type from the declaration, and delay the foreign
    -- export declaration
    --
    extType <- extractFunType pos cdecl isPure
    delayCode hook (foreignImport ideLexeme hsLexeme isUns extType)
    traceFunType extType
  where
    traceFunType et = traceGenBind $ 
      "Imported function type: " ++ showExtType et ++ "\n"

-- Haskell code for the foreign import declaration needed by a call hook
--
-- * appends a configuration dependent library suffix `dlsuffix'
--
foreignImport :: String -> String -> Bool -> ExtType -> String
foreignImport ident hsIdent isUnsafe ty  =
  "foreign import ccall " ++ safety ++ " \"" ++ ident ++ "\"\n  " ++ 
  hsIdent ++ " :: " ++ showExtType ty ++ "\n"
  where
    safety = if isUnsafe then "unsafe" else "safe"

-- produce a Haskell function definition for a fun hook
--
funDef :: Bool		     -- pure function?
       -> String	     -- name of the new Haskell function
       -> String	     -- Haskell name of the foreign imported C function
       -> CDecl		     -- simplified declaration of the C function
       -> Maybe String	     -- type context of the new Haskell function
       -> [CHSParm]	     -- parameter marhsalling description
       -> CHSParm	     -- result marshalling description 
       -> Position	     -- source location of the hook
       -> GB String	     -- Haskell code in text form
funDef isPure hsLexeme fiLexeme cdecl octxt parms parm pos =
  do
    (parms', parm', isImpure) <- addDftMarshaller pos parms parm cdecl
    traceMarsh parms' parm' isImpure
    let 
      sig       = hsLexeme ++ " :: " ++ funTy parms' parm' ++ "\n"
      marshs    = [marshArg i parm | (i, parm) <- zip [1..] parms']
      funArgs   = [funArg   | (funArg, _, _, _, _)   <- marshs, funArg   /= ""]
      marshIns  = [marshIn  | (_, marshIn, _, _, _)  <- marshs]
      callArgs  = [callArg  | (_, _, callArg, _, _)  <- marshs]
      marshOuts = [marshOut | (_, _, _, marshOut, _) <- marshs, marshOut /= ""]
      retArgs   = [retArg   | (_, _, _, _, retArg)   <- marshs, retArg   /= ""]
      funHead   = hsLexeme ++ join funArgs ++ " =\n" ++
	          if isPure && isImpure then "  unsafePerformIO $\n" else ""
      call      = if isPure 
		  then "  let {res = " ++ fiLexeme ++ join callArgs ++ "} in\n"
		  else "  " ++ fiLexeme ++ join callArgs ++ " >>= \\res ->\n"
      marshRes  = case parm' of
	            CHSParm _ _ twoCVal (Just (_    , CHSVoidArg)) _ -> ""
	            CHSParm _ _ twoCVal (Just (omIde, CHSIOArg  )) _ -> 
	              "  " ++ identToLexeme omIde ++ " res >>= \\res' ->\n"
	            CHSParm _ _ twoCVal (Just (omIde, CHSValArg )) _ -> 
	              "  let {res' = " ++ identToLexeme omIde ++ " res} in\n"
		    CHSParm _ _ _       Nothing		           _ ->
		      interr "GenBind.funDef: marshRes: no default?"
      retArgs'  = case parm' of
	            CHSParm _ _ _ (Just (_, CHSVoidArg)) _ ->        retArgs
	            _					   -> "res'":retArgs
      ret       = "(" ++ concat (intersperse ", " retArgs') ++ ")"
      funBody   = joinLines marshIns  ++ 
	          call                ++
	          joinLines marshOuts ++ 
		  marshRes            ++ 
		  "  " ++ 
		  (if isImpure || not isPure then "return " else "") ++ ret
    return $ sig ++ funHead ++ funBody
  where
    join      = concatMap (' ':)
    joinLines = concatMap (\s -> "  " ++ s ++ "\n")
    --
    -- construct the function type
    --
    -- * specified types appear in the argument and result only if their "in"
    --   and "out" marshaller, respectively, is not the `void' marshaller
    --
    funTy parms parm =
      let
        ctxt   = case octxt of
	           Nothing      -> ""
		   Just ctxtStr -> ctxtStr ++ " => "
	argTys = [ty | CHSParm im ty _ _  _ <- parms     , notVoid im]
        resTys = [ty | CHSParm _  ty _ om _ <- parm:parms, notVoid om]
        resTup = let
		   (lp, rp) = if isPure && length resTys == 1 
			      then ("", "") 
			      else ("(", ")") 
		   io       = if isPure then "" else "IO "
		 in
		 io ++ lp ++ concat (intersperse ", " resTys) ++ rp
		 
      in
      ctxt ++ concat (intersperse " -> " (argTys ++ [resTup]))
      where
        notVoid Nothing          = interr "GenBind.funDef: \
					  \No default marshaller?"
	notVoid (Just (_, kind)) = kind /= CHSVoidArg
    --
    -- for an argument marshaller, generate all "in" and "out" marshalling
    -- code fragments
    --
    marshArg i (CHSParm (Just (imIde, imArgKind)) _ twoCVal 
		        (Just (omIde, omArgKind)) _        ) =
      let
	a	 = "a" ++ show i
	imStr	 = identToLexeme imIde
	imApp	 = imStr ++ " " ++ a
	funArg   = if imArgKind == CHSVoidArg then "" else a
	inBndr   = if twoCVal 
		     then "(" ++ a ++ "'1, " ++ a ++ "'2)"
		     else a ++ "'"
	marshIn  = case imArgKind of
		     CHSVoidArg -> imStr ++ " $ \\" ++ inBndr ++ " -> "
		     CHSIOArg   -> imApp ++ " $ \\" ++ inBndr ++ " -> "
		     CHSValArg  -> "let {" ++ inBndr ++ " = " ++ 
				   imApp ++ "} in "
	callArg  = if twoCVal 
		     then "" ++ a ++ "'1 " ++ a ++ "'2"
		     else a ++ "'"
	omApp	 = identToLexeme omIde ++ " " ++ callArg
	outBndr  = a ++ "''"
        marshOut = case omArgKind of
		     CHSVoidArg -> ""
		     CHSIOArg   -> omApp ++ ">>= \\" ++ outBndr ++ " -> "
		     CHSValArg  -> "let {" ++ outBndr ++ " = " ++ 
				   omApp ++ "} in "
	retArg   = if omArgKind == CHSVoidArg then "" else outBndr
      in
      (funArg, marshIn, callArg, marshOut, retArg)
    marshArg _ _ = interr "GenBind.funDef: Missing default?"
    --
    traceMarsh parms parm isImpure = traceGenBind $ 
      "Marshalling specification including defaults: \n" ++
      showParms (parms ++ [parm]) "" ++
      "  The marshalling is " ++ if isImpure then "impure.\n" else "pure.\n"
      where
        showParms []           = id
	showParms (parm:parms) =   showString "  "
				 . showCHSParm parm 
				 . showChar '\n' 
				 . showParms parms

-- add default marshallers for "in" and "out" marshalling
--
addDftMarshaller :: Position -> [CHSParm] -> CHSParm -> CDecl 
		 -> GB ([CHSParm], CHSParm, Bool)
addDftMarshaller pos parms parm cdecl = do
  (resTy, argTys)     <- splitFunTy `liftM` extractFunType pos cdecl True
  (parm' , isImpure1) <- checkResMarsh parm resTy
  (parms', isImpure2) <- addDft parms argTys
  return (parms', parm', isImpure1 || isImpure2)
  where
    -- the result marshalling may not use an "in" marshaller and can only have
    -- one C value
    --
    -- * a default marshaller maybe used for "out" marshalling
    --
    checkResMarsh (CHSParm (Just _) _  _    _       pos) _   = 
      resMarshIllegalInErr      pos
    checkResMarsh (CHSParm _        _  True _       pos) _   = 
      resMarshIllegalTwoCValErr pos
    checkResMarsh (CHSParm _	    ty _    omMarsh pos) cTy = do
      (imMarsh', _       ) <- addDftVoid Nothing
      (omMarsh', isImpure) <- addDftOut pos omMarsh ty [cTy]
      return (CHSParm imMarsh' ty False omMarsh' pos, isImpure)
    --
    splitFunTy (FunET UnitET ty ) = splitFunTy ty
    splitFunTy (FunET ty1    ty2) = let 
				      (resTy, argTys) = splitFunTy ty2
				    in
				    (resTy, ty1:argTys)
    splitFunTy resTy	          = (resTy, [])
    --
    -- match Haskell with C arguments (and results)
    --
    addDft ((CHSParm imMarsh hsTy False omMarsh p):parms) (cTy      :cTys) = do
      (imMarsh', isImpureIn ) <- addDftIn   p imMarsh hsTy [cTy]
      (omMarsh', isImpureOut) <- addDftVoid    omMarsh
      (parms'  , isImpure   ) <- addDft parms cTys
      return (CHSParm imMarsh' hsTy False omMarsh' p : parms',
	      isImpure || isImpureIn || isImpureOut)
    addDft ((CHSParm imMarsh hsTy True  omMarsh p):parms) (cTy1:cTy2:cTys) = do
      (imMarsh', isImpureIn ) <- addDftIn   p imMarsh hsTy [cTy1, cTy2]
      (omMarsh', isImpureOut) <- addDftVoid   omMarsh
      (parms'  , isImpure   ) <- addDft parms cTys
      return (CHSParm imMarsh' hsTy True omMarsh' p : parms',
	      isImpure || isImpureIn || isImpureOut)
    addDft []                                             []               = 
      return ([], False)
    addDft ((CHSParm _       _    _     _     pos):parms) []               = 
      marshArgMismatchErr pos "This parameter is in excess of the C arguments."
    addDft []                                             (_:_)            = 
      marshArgMismatchErr pos "Parameter marshallers are missing."
    --
    addDftIn _   imMarsh@(Just (_, kind)) _    _    = return (imMarsh,
							      kind == CHSIOArg)
    addDftIn pos imMarsh@Nothing          hsTy cTys = do
      marsh <- lookupDftMarshIn hsTy cTys
      when (isNothing marsh) $
        noDftMarshErr pos "\"in\""
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    addDftOut _   omMarsh@(Just (_, kind)) _    _    = return (omMarsh,
							      kind == CHSIOArg)
    addDftOut pos omMarsh@Nothing          hsTy cTys = do
      marsh <- lookupDftMarshOut hsTy cTys
      when (isNothing marsh) $
        noDftMarshErr pos "\"out\""
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    -- add void marshaller if no explict one is given
    --
    addDftVoid marsh@(Just (_, kind)) = return (marsh, kind == CHSIOArg)
    addDftVoid        Nothing         = do
      return (Just (noPosIdent "void", CHSVoidArg), False)

-- compute from an access path, the declarator finally accessed and the index
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
accessPath :: CHSAPath -> GB (CDecl, [BitSize])
accessPath (CHSRoot ide) =				-- t
  do
    decl <- findAndChaseDecl ide False True
    return (ide `simplifyDecl` decl, [BitSize 0 0])
accessPath (CHSDeref (CHSRoot ide) _) =			-- *t
  do
    decl <- findAndChaseDecl ide True True
    return (ide `simplifyDecl` decl, [BitSize 0 0])
accessPath (CHSRef root@(CHSRoot ide1) ide2) =		-- t.m
  do
    su <- lookupStructUnion ide1 False True
    (offset, decl') <- refStruct su ide2
    adecl <- replaceByAlias decl'
    return (adecl, [offset])
accessPath (CHSRef (CHSDeref (CHSRoot ide1) _) ide2) =	-- t->m
  do
    su <- lookupStructUnion ide1 True True
    (offset, decl') <- refStruct su ide2
    adecl <- replaceByAlias decl'
    return (adecl, [offset])
accessPath (CHSRef path ide) =				-- a.m
  do
    (decl, offset:offsets) <- accessPath path
    assertPrimDeclr ide decl
    su <- structFromDecl (posOf ide) decl
    (addOffset, decl') <- refStruct su ide
    adecl <- replaceByAlias decl'
    return (adecl, offset `addBitSize` addOffset : offsets)
  where
    assertPrimDeclr ide (CDecl _ [declr] _) =
      case declr of
        (Just (CVarDeclr _ _), _, _) -> nop
	_			     -> structExpectedErr ide
accessPath (CHSDeref path pos) =			-- *a
  do
    (decl, offsets) <- accessPath path
    decl' <- derefOrErr decl
    adecl <- replaceByAlias decl'
    return (adecl, BitSize 0 0 : offsets)
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

-- replaces a decleration by its alias if any
--
-- * the alias inherits any field size specification that the original
--   declaration may have
--
-- * declaration must have exactly one declarator
--
replaceByAlias                                :: CDecl -> GB CDecl
replaceByAlias cdecl@(CDecl _ [(_, _, size)] at)  =
  do
    ocdecl <- checkForAlias cdecl
    case ocdecl of
      Nothing                                  -> return cdecl
      Just (CDecl specs [(declr, init, _)] at) ->   -- form of an alias
        return $ CDecl specs [(declr, init, size)] at

-- given a structure declaration and member name, compute the offset of the
-- member in the structure and the declaration of the referenced member
--
refStruct :: CStructUnion -> Ident -> GB (BitSize, CDecl)
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
		CUnionTag  -> return $ BitSize 0 0
    return (offset, decl)

-- does the given declarator define the given name?
--
declNamed :: CDecl -> Ident -> Bool
(CDecl _ [(Nothing   , _, _)] _) `declNamed` ide = False
(CDecl _ [(Just declr, _, _)] _) `declNamed` ide = declr `declrNamed` ide
(CDecl _ []                   _) `declNamed` _   =
  interr "GenBind.declNamed: Abstract declarator in structure!"
_				 `declNamed` _   =
  interr "GenBind.declNamed: More than one declarator!"

-- Haskell code for writing to or reading from a struct
--
setGet :: Position -> CHSAccess -> [BitSize] -> ExtType -> GB String
setGet pos access offsets ty =
  do
    let pre = case access of 
		CHSSet -> "(\\ptr val -> do {"
		CHSGet -> "(\\ptr -> do {"
    body <- setGetBody (reverse offsets)
    return $ pre ++ body ++ "})"
  where
    setGetBody [BitSize offset bitOffset] =
      do
	let tyTag = showExtType ty
        bf <- checkType ty
	case bf of
	  Nothing      -> return $ case access of	-- not a bitfield
			    CHSGet -> peekOp offset tyTag
			    CHSSet -> pokeOp offset tyTag "val"
--FIXME: must take `bitfieldDirection' into account
	  Just (_, bs) -> return $ case access of	-- a bitfield
			    CHSGet -> "val <- " ++ peekOp offset tyTag
				      ++ extractBitfield
			    CHSSet -> "org <- " ++ peekOp offset tyTag
				      ++ insertBitfield 
				      ++ pokeOp offset tyTag "val'"
	    where
	      -- we have to be careful here to ensure proper sign extension;
	      -- in particular, shifting right followed by anding a mask is
	      -- *not* sufficient; instead, we exploit in the following that
	      -- `shiftR' performs sign extension
	      --
	      extractBitfield = "; return $ (val `shiftL` (" 
				++ bitsPerField ++ " - " 
				++ show (bs + bitOffset) ++ ")) `shiftR` ("
				++ bitsPerField ++ " - " ++ show bs
				++ ")"
	      bitsPerField    = show $ size CIntPT * 8
	      --
	      insertBitfield  = "; let {val' = (org .&. " ++ middleMask
				++ ") .|. (val `shiftL` " 
				++ show bitOffset ++ ")}; "
	      middleMask      = "fromIntegral (((maxBound::CUInt) `shiftL` "
				++ show bs ++ ") `rotateL` " 
				++ show bitOffset ++ ")"
    setGetBody (BitSize offset 0 : offsets) =
      do
	code <- setGetBody offsets
	return $ "ptr <- peekByteOff ptr " ++ show offset ++ "; " ++ code
    setGetBody (BitSize _      _ : _      ) =
      derefBitfieldErr pos
    --
    -- check that the type can be marshalled and compute extra operations for
    -- bitfields
    --
    checkType (IOET      _    )          = interr "GenBind.setGet: Illegal \
						  \type!"
    checkType (UnitET         )          = voidFieldErr pos
    checkType (DefinedET _ _  )          = return Nothing-- can't check further
    checkType (PrimET    (CUFieldPT bs)) = return $ Just (False, bs)
    checkType (PrimET    (CSFieldPT bs)) = return $ Just (True , bs)
    checkType _		                 = return Nothing
    --
    peekOp off tyTag     = "peekByteOff ptr " ++ show off ++ " ::IO " ++ tyTag
    pokeOp off tyTag var = "pokeByteOff ptr " ++ show off ++ " (" ++ var
		           ++ "::" ++ tyTag ++ ")"

-- generate the type definition for a pointer hook and enter the required type
-- mapping into the `ptrmap'
--
pointerDef :: Bool		-- explicit `*' in pointer hook
	   -> Ident		-- full C name
	   -> String		-- Haskell name
	   -> CHSPtrType	-- kind of the pointer
	   -> Bool		-- explicit newtype tag
	   -> String		-- Haskell type expression of pointer argument
	   -> Bool		-- do we have a pointer to a function?
	   -> GB String
pointerDef isStar cNameFull hsName ptrKind isNewtype hsType isFun =
  do
    keepOld <- getSwitch oldFFI
    let ptrArg  = if keepOld 
		  then "()"		-- legacy FFI interface
		  else if isNewtype 
		  then hsName		-- abstract type
		  else hsType		-- concrete type
        ptrCon  = case ptrKind of
		    CHSPtr | isFun -> "FunPtr"
		    _              -> show ptrKind
	ptrType = ptrCon ++ " (" ++ ptrArg ++ ")"
	thePtr  = (isStar, cNameFull)
    case ptrKind of
      CHSForeignPtr -> thePtr `ptrMapsTo` ("Ptr (" ++ ptrArg ++ ")", 
					   "Ptr (" ++ ptrArg ++ ")")
      _		    -> thePtr `ptrMapsTo` (hsName, hsName)
    return $
      if isNewtype 
      then "newtype " ++ hsName ++ " = " ++ hsName ++ " (" ++ ptrType ++ ")" ++
	   withForeignFun
      else "type "    ++ hsName ++ " = "                   ++ ptrType
    where
      -- if we have a foreign pointer wrapped into a newtype, provide a
      -- safe unwrapping function automatically
      --
      withForeignFun 
        | ptrKind == CHSForeignPtr = 
	  "\n" ++
          "with" ++ hsName ++ " (" ++ hsName ++ " fptr) = withForeignPtr fptr"
	| otherwise		   = ""

-- generate the class and instance definitions for a class hook
--
-- * the pointer type must not be a stable pointer
--
-- * the first super class (if present) must be the direct superclass
--
-- * all Haskell objects in the superclass list must be pointer objects
--
classDef :: Position			 -- for error messages
	 -> String			 -- class name
	 -> String			 -- pointer type name
	 -> CHSPtrType			 -- type of the pointer
	 -> Bool			 -- is a newtype?
	 -> [(String, String, HsObject)] -- superclasses
	 -> GB String
classDef pos className typeName ptrType isNewtype superClasses =
  do
    let
      toMethodName    = case typeName of
		          ""   -> interr "GenBind.classDef: \
					 \Illegal identifier!"
			  c:cs -> toLower c : cs
      fromMethodName  = "from" ++ typeName
      classDefContext = case superClasses of
			  []                  -> "" 
			  (superName, _, _):_ -> superName ++ " p => "
      classDef        = 
        "class " ++ classDefContext ++ className ++ " p where\n" 
	++ "  " ++ toMethodName   ++ " :: p -> " ++ typeName ++ "\n"
	++ "  " ++ fromMethodName ++ " :: " ++ typeName ++ " -> p\n"
      instDef	      = 
        "instance " ++ className ++ " " ++ typeName ++ " where\n"
	++ "  " ++ toMethodName   ++ " = id\n"
	++ "  " ++ fromMethodName ++ " = id\n"
    instDefs <- castInstDefs superClasses
    return $ classDef ++ instDefs ++ instDef
  where 
    castInstDefs [] = return ""
    castInstDefs ((superName, ptrName, Pointer ptrType' isNewtype'):classes) =
      do
	unless (ptrType == ptrType') $
	  pointerTypeMismatchErr pos className superName
        let toMethodName    = case ptrName of
		                ""   -> interr "GenBind.classDef: \
					 \Illegal identifier - 2!"
			        c:cs -> toLower c : cs
            fromMethodName  = "from" ++ ptrName
	    castFun	    = "cast" ++ show ptrType
	    typeConstr      = if isNewtype  then typeName ++ " " else ""
	    superConstr     = if isNewtype' then ptrName  ++ " " else ""
	    instDef         =
	      "instance " ++ superName ++ " " ++ typeName ++ " where\n"
	      ++ "  " ++ toMethodName     ++ " (" ++ typeConstr  ++ "p) = " 
	        ++ superConstr ++ "(" ++ castFun ++ " p)\n"
	      ++ "  " ++ fromMethodName   ++ " (" ++ superConstr ++ "p) = " 
	        ++ typeConstr  ++ "(" ++ castFun ++ " p)\n"
	instDefs <- castInstDefs classes
	return $ instDef ++ instDefs


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
--   structure; an aliased type is always a pointer type that is contained in
--   the pointer map (and got there either from a .chi or from a pointer hook
--   in the same module)
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

instance Eq ExtType where
  (FunET     t1 t2) == (FunET     t1' t2') = t1 == t1' && t2 == t2'
  (IOET      t    ) == (IOET      t'     ) = t == t'
  (PtrET     t    ) == (PtrET     t'     ) = t == t'
  (DefinedET _  s ) == (DefinedET _   s' ) = s == s'
  (PrimET    t    ) == (PrimET    t'     ) = t == t'
  UnitET	    == UnitET		   = True

-- composite C type
--
data CompType = ExtType  ExtType		-- external type
	      | SUType	 CStructUnion		-- structure or union

-- check whether an external type denotes a function type
--
isFunExtType             :: ExtType -> Bool
isFunExtType (FunET _ _)  = True
isFunExtType (IOET  _  )  = True
isFunExtType _            = False

-- pretty print an external type
--
-- * a previous version of this function attempted to not print unnecessary
--   brackets; this however doesn't work consistently due to `DefinedET'; so,
--   we give up on the idea (preferring simplicity)
--
showExtType                        :: ExtType -> String
showExtType (FunET UnitET res)      = showExtType res
showExtType (FunET arg res)	    = "(" ++ showExtType arg ++ " -> " 
				      ++ showExtType res ++ ")"
showExtType (IOET t)		    = "(IO " ++ showExtType t ++ ")"
showExtType (PtrET t)	            = let ptrCon = if isFunExtType t 
						   then "FunPtr" else "Ptr"
				      in
				      "(" ++ ptrCon ++ " " ++ showExtType t 
				      ++ ")"
showExtType (DefinedET _ str)       = "(" ++ str ++ ")"
showExtType (PrimET CPtrPT)         = "(Ptr ())"
showExtType (PrimET CFunPtrPT)      = "(FunPtr ())"
showExtType (PrimET CCharPT)        = "CChar"
showExtType (PrimET CUCharPT)       = "CUChar"
showExtType (PrimET CSCharPT)       = "CSChar"
showExtType (PrimET CIntPT)         = "CInt"
showExtType (PrimET CShortPT)       = "CShort"
showExtType (PrimET CLongPT)        = "CLong"
showExtType (PrimET CLLongPT)       = "CLLong"
showExtType (PrimET CUIntPT)        = "CUInt"
showExtType (PrimET CUShortPT)      = "CUShort"
showExtType (PrimET CULongPT)       = "CULong"
showExtType (PrimET CULLongPT)      = "CULLong"
showExtType (PrimET CFloatPT)       = "CFloat"
showExtType (PrimET CDoublePT)      = "CDouble"
showExtType (PrimET CLDoublePT)     = "CLDouble"
showExtType (PrimET (CSFieldPT bs)) = "CInt{-:" ++ show	bs ++ "-}"
showExtType (PrimET (CUFieldPT bs)) = "CUInt{-:" ++ show bs ++ "-}"
showExtType UnitET		    = "()"

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
    traceEnter
    ct <- extractCompType isResult cdecl
    case ct of
      ExtType et -> return et
      SUType  _  -> illegalStructUnionErr (posOf cdecl) pos
  where
    traceEnter = traceGenBind $ 
      "Entering `extractSimpleType' (" ++ (if isResult then "" else "not ") 
      ++ "for a result)...\n"

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
--   function (this is only applicable to direct results and not to type
--   parameters for pointers that are a result)
--
-- * takes the pointer map into account
--
-- * IMPORTANT NOTE: `sizeAlignOf' relies on `DefinedET' only being produced
--		     for pointer types; if this ever changes, we need to
--		     handle `DefinedET's differently.  The problem is that
--		     entries in the pointer map currently prevent
--		     `extractCompType' from looking further "into" the
--		     definition of that pointer.
--
extractCompType :: Bool -> CDecl -> GB CompType
extractCompType isResult cdecl@(CDecl specs declrs ats)  = 
  if length declrs > 1 
  then interr "GenBind.extractCompType: Too many declarators!"
  else case declrs of
    [(Just declr, _, size)] | isPtrDeclr declr -> ptrType declr
			    | isFunDeclr declr -> funType
			    | otherwise	       -> aliasOrSpecType size
    []					       -> aliasOrSpecType Nothing
  where
    -- handle explicit pointer types
    --
    ptrType declr = do
      tracePtrType
      let declrs' = dropPtrDeclr declr		-- remove indirection
	  cdecl'  = CDecl specs [(Just declrs', Nothing, Nothing)] ats
          oalias  = checkForOneAliasName cdecl' -- is only an alias remaining?
      oHsRepr <- case oalias of
		   Nothing  -> return $ Nothing
		   Just ide -> queryPtr (True, ide)
      case oHsRepr of
        Just repr  -> ptrAlias repr             -- got an alias
	Nothing    -> do			-- no alias => recurs
	  ct <- extractCompType False cdecl'
	  returnX $ case ct of
		      ExtType et -> PtrET et
		      SUType  _  -> PtrET UnitET
    --
    -- handle explicit function types
    --
    -- FIXME: we currently regard any functions as being impure (ie, being IO
    --	      functions); is this ever going to be a problem?
    --
    funType = do
	        traceFunType
	        et <- extractFunType (posOf cdecl) cdecl False
		returnX et
    --
    -- handle all types, which are not obviously pointers or functions 
    --
    aliasOrSpecType :: Maybe CExpr -> GB CompType
    aliasOrSpecType size = do
      traceAliasOrSpecType size
      case checkForOneAliasName cdecl of
        Nothing   -> specType (posOf cdecl) specs size
	Just ide  -> do                    -- this is a typedef alias
	  traceAlias ide
	  oHsRepr <- queryPtr (False, ide) -- check for pointer hook alias     
	  case oHsRepr of
	    Nothing   -> do		   -- skip current alias (only one)
			   cdecl' <- getDeclOf ide
			   let CDecl specs [(declr, init, _)] at =
			         ide `simplifyDecl` cdecl'
                               sdecl = CDecl specs [(declr, init, size)] at
			       -- propagate `size' down (slightly kludgy)
			   extractCompType isResult sdecl
	    Just repr -> ptrAlias repr     -- found a pointer hook alias
    --
    -- compute the result for a pointer alias
    --
    ptrAlias (repr1, repr2) = 
      returnX $ DefinedET cdecl (if isResult then repr2 else repr1)
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
    --
    tracePtrType = traceGenBind $ "extractCompType: explicit pointer type\n"
    traceFunType = traceGenBind $ "extractCompType: explicit function type\n"
    traceAliasOrSpecType Nothing  = traceGenBind $ 
      "extractCompType: checking for alias\n"
    traceAliasOrSpecType (Just _) = traceGenBind $ 
      "extractCompType: checking for alias of bitfield\n"
    traceAlias ide = traceGenBind $ 
      "extractCompType: found an alias called `" ++ identToLexeme ide ++ "'\n"

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
specType :: Position -> [CDeclSpec] -> Maybe CExpr -> GB CompType
specType cpos specs osize = 
  let tspecs = [ts | CTypeSpec ts <- specs]
  in case lookupTSpec tspecs typeMap of
    Just et | isUnsupportedType et -> unsupportedTypeSpecErr cpos
	    | isNothing osize	   -> return $ ExtType et     -- not a bitfield
	    | otherwise		   -> bitfieldSpec tspecs et osize  -- bitfield
    Nothing                        -> 
      case tspecs of
	[CSUType   cu _] -> return $ SUType cu               -- struct or union
	[CEnumType _  _] -> return $ ExtType (PrimET CIntPT) -- enum
	[CTypeDef  _  _] -> interr "GenBind.specType: Illegal typedef alias!"
	_		 -> illegalTypeSpecErr cpos
  where
    lookupTSpec = lookupBy matches
    --
    isUnsupportedType (PrimET et) = size et == 0  -- can't be a bitfield (yet)
    isUnsupportedType _		  = False
    --
    -- check whether two type specifier lists denote the same type; handles
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
    --
    bitfieldSpec :: [CTypeSpec] -> ExtType -> Maybe CExpr -> GB CompType
    bitfieldSpec tspecs et (Just sizeExpr) =  -- never called with `Nothing'
      do
        let pos = posOf sizeExpr
	sizeResult <- evalConstCExpr sizeExpr
	case sizeResult of
	  FloatResult _     -> illegalConstExprErr pos "a float result"
	  IntResult   size' -> do
	    let size = fromInteger size'
	    case et of
	      PrimET CUIntPT                      -> returnCT $ CUFieldPT size
	      PrimET CIntPT 
	        |  [signed]      `matches` tspecs 
		|| [signed, int] `matches` tspecs -> returnCT $ CSFieldPT size
		|  [int]         `matches` tspecs -> 
		  returnCT $ if bitfieldIntSigned then CSFieldPT size 
						  else CUFieldPT size
	      _			 		  -> illegalFieldSizeErr pos
	    where
	      returnCT = return . ExtType . PrimET
	      --
	      int    = CIntType    undefined
	      signed = CSignedType undefined


-- offset and size computations
-- ----------------------------

-- precise size representation
--
-- * this is a pair of a number of octets and a number of bits
--
-- * if the number of bits is nonzero, the octet component is aligned by the
--   alignment constraint for `CIntPT' (important for accessing bitfields with
--   more than 8 bits)
--
data BitSize = BitSize Int Int
	     deriving (Eq, Show)

-- ordering relation compares in terms of required storage units
--
instance Ord BitSize where
  bs1@(BitSize o1 b1) <  bs2@(BitSize o2 b2) = 
    padBits bs1 < padBits bs2 || (o1 == o2 && b1 < b2)
  bs1                 <= bs2		     = bs1 < bs2 || bs1 == bs2
    -- the <= instance is needed for Ord's compare functions, which is used in
    -- the defaults for all other members

-- add two bit size values
--
addBitSize                                 :: BitSize -> BitSize -> BitSize
addBitSize (BitSize o1 b1) (BitSize o2 b2)  = BitSize (o1 + o2 + overflow) rest
  where
    bitsPerBitfield  = size CIntPT * 8
    (overflow, rest) = (b1 + b2) `divMod` bitsPerBitfield

-- pad any storage unit that is partially used by a bitfield
--
padBits               :: BitSize -> Int
padBits (BitSize o 0)  = o
padBits (BitSize o _)  = o + size CIntPT

-- compute the offset of the declarator in the second argument when it is
-- preceded by the declarators in the first argument
--
offsetInStruct                :: [CDecl] -> CDecl -> CStructTag -> GB BitSize
offsetInStruct []    _    _    = return $ BitSize 0 0
offsetInStruct decls decl tag  = 
  do
    (offset, _) <- sizeAlignOfStruct decls tag
    (_, align)  <- sizeAlignOf decl
    return $ alignOffset offset align

-- compute the size and alignment (no padding at the end) of a set of
-- declarators from a struct
--
sizeAlignOfStruct :: [CDecl] -> CStructTag -> GB (BitSize, Int)
sizeAlignOfStruct []    _           = return (BitSize 0 0, 1)
sizeAlignOfStruct decls CStructTag  = 
  do
    (offset, preAlign) <- sizeAlignOfStruct (init decls) CStructTag
    (size, align)      <- sizeAlignOf       (last decls)
    let sizeOfStruct  = alignOffset offset align `addBitSize` size
	align'	      = if align > 0 then align else bitfieldAlignment
	alignOfStruct = preAlign `max` align'
    return (sizeOfStruct, alignOfStruct)
sizeAlignOfStruct decls CUnionTag   =
  do
    (sizes, aligns) <- mapAndUnzipM sizeAlignOf decls
    let aligns' = [if align > 0 then align else bitfieldAlignment
		  | align <- aligns]
    return (maximum sizes, maximum aligns')

-- compute the size and alignment of the declarators forming a struct
-- including any end-of-struct padding that is needed to make the struct ``tile
-- in an array'' (K&R A7.4.8)
--
sizeAlignOfStructPad :: [CDecl] -> CStructTag -> GB (BitSize, Int)
sizeAlignOfStructPad decls tag =
  do
    (size, align) <- sizeAlignOfStruct decls tag
    return (alignOffset size align, align)

-- compute the size and alignment constraint of a given C declaration
--
sizeAlignOf       :: CDecl -> GB (BitSize, Int)
--
-- * we make use of the assertion that `extractCompType' can only return a
--   `DefinedET' when the declaration is a pointer declaration
--
sizeAlignOf cdecl  = 
  do
    ct <- extractCompType False cdecl
    case ct of
      ExtType (FunET _ _        ) -> return (bitSize CFunPtrPT, 
					     alignment CFunPtrPT)
      ExtType (IOET  _          ) -> interr "GenBind.sizeof: Illegal IO type!"
      ExtType (PtrET t          ) 
        | isFunExtType t          -> return (bitSize CFunPtrPT, 
					     alignment CFunPtrPT)
        | otherwise		  -> return (bitSize CPtrPT, alignment CPtrPT)
      ExtType (DefinedET _ _    ) -> return (bitSize CPtrPT, alignment CPtrPT)
        -- FIXME: The defined type could be a function pointer!!!
      ExtType (PrimET pt        ) -> return (bitSize pt, alignment pt)
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
	  sizeAlignOfStructPad fields' tag
  where
    bitSize et | sz < 0    = BitSize 0  (-sz)	-- size is in bits
	       | otherwise = BitSize sz 0
	       where
	         sz = size et

-- apply the given alignment constraint at the given offset
--
-- * if the alignment constraint is negative or zero, it is the alignment
--   constraint for a bitfield
--
alignOffset :: BitSize -> Int -> BitSize
alignOffset offset@(BitSize octetOffset bitOffset) align 
  | align > 0 && bitOffset /= 0 =		-- close bitfield first
    alignOffset (BitSize (octetOffset + (bitOffset + 7) `div` 8) 0) align
  | align > 0 && bitOffset == 0 =	        -- no bitfields involved
    BitSize (((octetOffset - 1) `div` align + 1) * align) 0
  | bitOffset == 0 	        	        -- start a bitfield
    || overflowingBitfield	=		-- .. or overflowing bitfield
    alignOffset offset bitfieldAlignment
  | otherwise			=		-- stays in current bitfield
    offset
  where
    bitsPerBitfield     = size CIntPT * 8
    overflowingBitfield = bitOffset - align >= bitsPerBitfield
				    -- note, `align' is negative


-- constant folding
-- ----------------

-- evaluate a constant expression
--
-- FIXME: this is a bit too simplistic, as the range of expression allowed as
--	  constant expression varies depending on the context in which the
--	  constant expression occurs
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
evalConstCExpr (CSizeofType decl _) =
  do
    (size, _) <- sizeAlignOf decl
    return $ IntResult (fromIntegral . padBits $ size)
evalConstCExpr (CAlignofExpr _ _) =
  todo "GenBind.evalConstCExpr: alignof (GNU C extension) not implemented yet."
evalConstCExpr (CAlignofType decl _) =
  do
    (_, align) <- sizeAlignOf decl
    return $ IntResult (fromIntegral align)
evalConstCExpr (CIndex _ _ at) =
  illegalConstExprErr (posOf at) "array indexing"
evalConstCExpr (CCall _ _ at) =
  illegalConstExprErr (posOf at) "function call"
evalConstCExpr (CMember _ _ _ at) =
  illegalConstExprErr (posOf at) "a . or -> operator"
evalConstCExpr (CVar ide at) =
  do
    (cobj, _) <- findValueObj ide False
    case cobj of
      EnumCO ide (CEnum _ enumrs _) -> liftM IntResult $ 
				         enumTagValue ide enumrs 0
      _		                    -> 
        todo $ "GenBind.evalConstCExpr: variable names not implemented yet " ++
	       show (posOf at)
  where
    -- FIXME: this is not very nice; instead, CTrav should have some support
    --	      for determining enum tag values (but then, constant folding needs
    --	      to be moved to CTrav, too)
    --
    -- Compute the tag value for `ide' defined in the given enumerator list
    --
    enumTagValue _   []                     _   = 
      interr "GenBind.enumTagValue: enumerator not in declaration"
    enumTagValue ide ((ide', oexpr):enumrs) val =
      do
	val' <- case oexpr of
		  Nothing  -> return val
		  Just exp -> 
		    do
		      val' <- evalConstCExpr exp
		      case val' of
			IntResult val' -> return val'
			FloatResult _  ->
			  illegalConstExprErr (posOf exp) "a float result"
	if ide == ide'
	  then			-- found the right enumerator
	    return val'
	  else			-- continue down the enumerator list
	    enumTagValue ide enumrs (val' + 1)
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


-- auxilliary functions
-- --------------------

-- create an identifier without position information
--
noPosIdent :: String -> Ident
noPosIdent  = onlyPosIdent nopos

-- print trace message
--
traceGenBind :: String -> GB ()
traceGenBind  = putTraceStr traceGenBindSW


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

illegalStablePtrErr     :: Position -> GB a
illegalStablePtrErr pos  =
  raiseErrorCTExc pos
    ["Illegal use of a stable pointer!",
     "Class hooks cannot be used for stable pointers."]

pointerTypeMismatchErr :: Position -> String -> String -> GB a
pointerTypeMismatchErr pos className superName =
  raiseErrorCTExc pos
    ["Pointer type mismatch!",
     "The pointer of the class hook for `" ++ className 
     ++ "' is of a different kind",
     "than that of the class hook for `" ++ superName ++ "'; this is illegal",
     "as the latter is defined to be an (indirect) superclass of the former."]

illegalFieldSizeErr      :: Position -> GB a
illegalFieldSizeErr cpos  =
  raiseErrorCTExc cpos 
    ["Illegal field size!",
     "Only signed and unsigned `int' types may have a size annotation."]

derefBitfieldErr      :: Position -> GB a
derefBitfieldErr pos  =
  raiseErrorCTExc pos 
    ["Illegal dereferencing of a bit field!",
     "Bit fields cannot be dereferenced."]

resMarshIllegalInErr     :: Position -> GB a
resMarshIllegalInErr pos  =
  raiseErrorCTExc pos 
    ["Malformed result marshalling!",
     "There may not be an \"in\" marshaller for the result."]

resMarshIllegalTwoCValErr     :: Position -> GB a
resMarshIllegalTwoCValErr pos  =
  raiseErrorCTExc pos 
    ["Malformed result marshalling!",
     "Two C values (i.e., the `&' symbol) are not allowed for the result."]

marshArgMismatchErr            :: Position -> String -> GB a
marshArgMismatchErr pos reason  =
  raiseErrorCTExc pos
    ["Function arity mismatch!",
     reason]

noDftMarshErr           :: Position -> String -> GB a
noDftMarshErr pos inOut  =
  raiseErrorCTExc pos
    ["Missing " ++ inOut ++ " marshaller!",
     "There is no default marshaller for this combination of Haskell and \
     \C type available."]
