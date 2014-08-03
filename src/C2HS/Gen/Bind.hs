--  C->Haskell Compiler: binding generator
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
--- Description ---------------------------------------------------------------
--
--  Language: Haskell 98
--
--  Module implementing the expansion of the binding hooks.
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
--    char                      -> CChar
--    unsigned char             -> CUChar
--    signed char               -> CShort
--    signed                    -> CInt
--    [signed] int              -> CInt
--    [signed] short [int]      -> CSInt
--    [signed] long [int]       -> CLong
--    [signed] long long [int]  -> CLLong
--    unsigned [int]            -> CUInt
--    unsigned short [int]      -> CUShort
--    unsigned long [int]       -> CULong
--    unsigned long long [int]  -> CULLong
--    float                     -> CFloat
--    double                    -> CDouble
--    long double               -> CLDouble
--    enum ...                  -> CInt
--    struct ...                -> ** error **
--    union ...                 -> ** error **
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
--- ToDo ----------------------------------------------------------------------
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

module C2HS.Gen.Bind (expandHooks)
where

import Prelude hiding (exp, lookup)

-- standard libraries
import Data.Char     (toLower)
import Data.Function (on)
import Data.List     (deleteBy, groupBy, sortBy, intersperse, find, nubBy, intercalate)
import Data.Map      (lookup)
import Data.Maybe    (isNothing, isJust, fromJust, fromMaybe)
import Data.Bits     ((.|.), (.&.))
import Control.Arrow (second)
import Control.Monad (when, unless, liftM, mapAndUnzipM)
import Data.Ord      (comparing)

-- Language.C / compiler toolkit
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Pretty
import Text.PrettyPrint.HughesPJ (render)
import Data.Errors

-- C->Haskell
import C2HS.Config (PlatformSpec(..))
import C2HS.State  (CST, errorsPresent, showErrors, fatal,
                   SwitchBoard(..), Traces(..), putTraceStr, getSwitch)
import C2HS.C

-- friends
import C2HS.CHS   (CHSModule(..), CHSFrag(..), CHSHook(..),
                   CHSParm(..), CHSMarsh, CHSArg(..), CHSAccess(..), CHSAPath(..),
                   CHSPtrType(..), showCHSParm, apathToIdent)
import C2HS.C.Info      (CPrimType(..), alignment, getPlatform)
import qualified C2HS.C.Info as CInfo
import C2HS.Gen.Monad    (TransFun, transTabToTransFun, HsObject(..), GB,
                          GBState(..),
                   initialGBState, setContext, getPrefix, getReplacementPrefix,
                   delayCode, getDelayedCode, ptrMapsTo, queryPtr, objIs,
                   queryClass, queryPointer, mergeMaps, dumpMaps,
                   queryEnum, isEnum)


-- default marshallers
-- -------------------

-- FIXME:
-- - we might have a dynamically extended table in the monad if needed (we
--   could marshall enums this way and also save the 'id' marshallers for
--   pointers defined via (newtype) pointer hooks)
-- - the checks for the Haskell types are quite kludgy

stringIn :: String
stringIn = "\\s f -> withCStringLen s " ++
           "(\\(p, n) -> f (p, fromIntegral n))"

-- | determine the default "in" marshaller for the given Haskell and C types
--
lookupDftMarshIn :: String -> [ExtType] -> GB CHSMarsh
lookupDftMarshIn "Bool"   [PrimET pt] | isIntegralCPrimType pt =
  return $ Just (Left cFromBoolIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isIntegralHsType hsTy
                                      &&isIntegralCPrimType pt =
  return $ Just (Left cIntConvIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isFloatHsType hsTy
                                      &&isFloatCPrimType pt    =
  return $ Just (Left cFloatConvIde, CHSValArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT)]             =
  return $ Just (Left withCStringIde, CHSIOArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT), PrimET pt]
  | isIntegralCPrimType pt                                     =
  return $ Just (Right stringIn , CHSIOArg)
lookupDftMarshIn hsTy     [PtrET ty]  | showExtType ty == hsTy =
  return $ Just (Right stringIn, CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]
  | isIntegralHsType hsTy && isIntegralCPrimType pt            =
  return $ Just (Right "with . fromIntegral", CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]
  | isFloatHsType hsTy && isFloatCPrimType pt                  =
  return $ Just (Right "with . realToFrac", CHSIOArg)
lookupDftMarshIn "Bool"   [PtrET (PrimET pt)]
  | isIntegralCPrimType pt                                     =
  return $ Just (Right "with . fromBool", CHSIOArg)
-- Default case deals with:
lookupDftMarshIn hsty _ = do
  om <- readCT objmap
  isenum <- queryEnum hsty
  return $ case (isenum, (internalIdent hsty) `lookup` om) of
    --  1. enumeration hooks
    (True, Nothing) -> Just (Right "fromIntegral . fromEnum", CHSValArg)
    --  2. naked and newtype pointer hooks
    (False, Just (Pointer CHSPtr _)) -> Just (Left idIde, CHSValArg)
    --  3. foreign pointer hooks
    (False, Just (Pointer CHSForeignPtr False)) ->
      Just (Left withForeignPtrIde, CHSIOArg)
    --  4. foreign newtype pointer hooks
    (False, Just (Pointer CHSForeignPtr True)) ->
      Just (Right $ "with" ++ hsty, CHSIOArg)
    _ -> Nothing
-- FIXME: handle array-list conversion


-- | determine the default "out" marshaller for the given Haskell and C types
--
lookupDftMarshOut :: String -> [ExtType] -> GB CHSMarsh
lookupDftMarshOut "()"     _                                    =
  return $ Just (Left voidIde, CHSVoidArg)
lookupDftMarshOut "Bool"   [PrimET pt] | isIntegralCPrimType pt =
  return $ Just (Left cToBoolIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isIntegralHsType hsTy
                                       &&isIntegralCPrimType pt =
  return $ Just (Left cIntConvIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isFloatHsType hsTy
                                       &&isFloatCPrimType pt    =
  return $ Just (Left cFloatConvIde, CHSValArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT)]             =
  return $ Just (Left peekCStringIde, CHSIOArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT), PrimET pt]
  | isIntegralCPrimType pt                                      =
  return $ Just (Right "\\(s, n) -> peekCStringLen (s, fromIntegral n)",
                 CHSIOArg)
lookupDftMarshOut hsTy     [PtrET ty]  | showExtType ty == hsTy =
  return $ Just (Left peekIde, CHSIOArg)
-- Default case deals with:
lookupDftMarshOut hsty _ = do
  om <- readCT objmap
  isenum <- queryEnum hsty
  return $ case (isenum, (internalIdent hsty) `lookup` om) of
    --  1. enumeration hooks
    (True, Nothing) -> Just (Right "toEnum . fromIntegral", CHSValArg)
    --  2. naked and newtype pointer hooks
    (False, Just (Pointer CHSPtr _)) -> Just (Left idIde, CHSValArg)
    --  3. foreign pointer hooks
    (False, Just (Pointer CHSForeignPtr False)) ->
      Just (Left newForeignPtrIde, CHSIOArg)
    --  4. foreign newtype pointer hooks
    (False, Just (Pointer CHSForeignPtr True)) ->
      Just (Right $ "newForeignPtr_ >=> (return . " ++ hsty ++ ")", CHSIOArg)
    _ -> Nothing
-- FIXME: add combination, such as "peek" plus "cIntConv" etc
-- FIXME: handle array-list conversion


-- | check for integral Haskell types
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
isIntegralHsType _        = False

-- | check for floating Haskell types
--
isFloatHsType :: String -> Bool
isFloatHsType "Float"  = True
isFloatHsType "Double" = True
isFloatHsType _        = False

isVariadic :: ExtType -> Bool
isVariadic (FunET s t)  = any isVariadic [s,t]
isVariadic (IOET t)     = isVariadic t
isVariadic (PtrET t)    = isVariadic t
isVariadic (VarFunET _) = True
isVariadic _            = False

-- | check for integral C types
--
-- * For marshalling purposes C char's are integral types (see also types
--   classes for which the FFI guarantees instances for 'CChar', 'CSChar', and
--   'CUChar')
--
isIntegralCPrimType :: CPrimType -> Bool
isIntegralCPrimType  = (`elem` [CCharPT, CSCharPT, CIntPT, CShortPT, CLongPT,
                                CLLongPT, CUIntPT, CUCharPT, CUShortPT,
                                CULongPT, CULLongPT])

-- | check for floating C types
--
isFloatCPrimType :: CPrimType -> Bool
isFloatCPrimType  = (`elem` [CFloatPT, CDoublePT, CLDoublePT])

-- | standard conversions
--
voidIde, cFromBoolIde, cToBoolIde, cIntConvIde, cFloatConvIde,
  withCStringIde, peekIde, peekCStringIde, idIde,
  newForeignPtrIde, withForeignPtrIde :: Ident
voidIde           = internalIdent "void"         -- never appears in the output
cFromBoolIde      = internalIdent "fromBool"
cToBoolIde        = internalIdent "toBool"
cIntConvIde       = internalIdent "fromIntegral"
cFloatConvIde     = internalIdent "realToFrac"
withCStringIde    = internalIdent "withCString"
peekIde           = internalIdent "peek"
peekCStringIde    = internalIdent "peekCString"
idIde             = internalIdent "id"
newForeignPtrIde  = internalIdent "newForeignPtr_"
withForeignPtrIde = internalIdent "withForeignPtr"


-- expansion of binding hooks
-- --------------------------

-- | given a C header file and a binding file, expand all hooks in the binding
-- file using the C header information
--
-- * together with the module, returns the contents of the .chi file
--
-- * if any error (not warnings) is encountered, a fatal error is raised.
--
-- * also returns all warning messages encountered (last component of result)
--
expandHooks        :: AttrC -> CHSModule -> CST s (CHSModule, String, String)
expandHooks ac mod'  = do
                        (_, res) <- runCT (expandModule mod') ac initialGBState
                        return res

expandModule                   :: CHSModule -> GB (CHSModule, String, String)
expandModule (CHSModule mfrags)  =
  do
    -- expand hooks
    --
    traceInfoExpand
    frags'       <- expandFrags mfrags
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
expandFrag line@(CHSLine _       ) = return [line]
expandFrag      (CHSHook h    pos) =
  do
    code <- expandHook h pos
    return [CHSVerb code builtinPos]
  `ifCTExc` return [CHSVerb "** ERROR **" builtinPos]
expandFrag      (CHSCPP  s _    _) =
  interr $ "GenBind.expandFrag: Left over CHSCPP!\n---\n" ++ s ++ "\n---"
expandFrag      (CHSC    s _     ) =
  interr $ "GenBind.expandFrag: Left over CHSC!\n---\n" ++ s ++ "\n---"
expandFrag      (CHSCond alts dft) =
  do
    traceInfoCond
    select alts
  where
    select []                   = do
                                    traceInfoDft dft
                                    expandFrags (maybe [] id dft)
    select ((ide, cfrags):alts') = do
                                    oobj <- findTag ide
                                    traceInfoVal ide oobj
                                    if isNothing oobj
                                      then
                                        select alts'
                                      else            -- found right alternative
                                        expandFrags cfrags
    --
    traceInfoCond         = traceGenBind "** CPP conditional:\n"
    traceInfoVal ide oobj = traceGenBind $ identToString ide ++ " is " ++
                              (if isNothing oobj then "not " else "") ++
                              "defined.\n"
    traceInfoDft dft'     = if isNothing dft'
                            then
                              return ()
                            else
                              traceGenBind "Choosing else branch.\n"

expandHook :: CHSHook -> Position -> GB String
expandHook (CHSImport qual ide chi _) _ =
  do
    mergeMaps chi
    return $
      "import " ++ (if qual then "qualified " else "") ++ identToString ide
expandHook (CHSContext olib oprefix orepprefix _) _ =
  do
    setContext olib oprefix orepprefix         -- enter context information
    -- use the prefix on name spaces
    when (isJust oprefix) $
      applyPrefixToNameSpaces (fromJust oprefix) (maybe "" id orepprefix)
    return ""
expandHook (CHSNonGNU _) _ = return ""
expandHook (CHSType ide pos) _ =
  do
    traceInfoType
    decl <- findAndChaseDecl ide False True     -- no indirection, but shadows
    ty <- extractSimpleType False pos decl
    traceInfoDump (render $ pretty decl) ty
    when (isVariadic ty) (variadicErr pos (posOf decl))
    return $ "(" ++ showExtType ty ++ ")"
  where
    traceInfoType         = traceGenBind "** Type hook:\n"
    traceInfoDump decl ty = traceGenBind $
      "Declaration\n" ++ show decl ++ "\ntranslates to\n"
      ++ showExtType ty ++ "\n"
expandHook (CHSAlignof ide _) _ =
  do
    traceInfoAlignof
    decl <- findAndChaseDecl ide False True     -- no indirection, but shadows
    (_, align) <- sizeAlignOf decl
    traceInfoDump (render $ pretty decl) align
    return $ show align
  where
    traceInfoAlignof         = traceGenBind "** alignment hook:\n"
    traceInfoDump decl align = traceGenBind $
      "Alignment of declaration\n" ++ show decl ++ "\nis "
      ++ show align ++ "\n"

expandHook (CHSSizeof ide _) _ =
  do
    traceInfoSizeof
    decl <- findAndChaseDecl ide False True     -- no indirection, but shadows
    (size, _) <- sizeAlignOf decl
    traceInfoDump (render $ pretty decl) size
    return $ show (padBits size)
  where
    traceInfoSizeof         = traceGenBind "** Sizeof hook:\n"
    traceInfoDump decl size = traceGenBind $
      "Size of declaration\n" ++ show decl ++ "\nis "
      ++ show (padBits size) ++ "\n"
expandHook (CHSEnumDefine _ _ _ _) _ =
  interr "Binding generation error : enum define hooks should be eliminated via preprocessing "
expandHook (CHSEnum cide oalias chsTrans oprefix orepprefix derive pos) _ =
  do
    -- get the corresponding C declaration
    --
    enum <- lookupEnum cide True        -- smart lookup incl error handling
    --
    -- convert the translation table and generate data type definition code
    --
    gprefix <- getPrefix
    let pfx = case oprefix of
          Nothing -> gprefix
          Just pref -> pref
    grepprefix <- getReplacementPrefix
    let reppfx = case orepprefix of
          Nothing -> grepprefix
          Just pref -> pref

    let trans = transTabToTransFun pfx reppfx chsTrans
        hide  = identToString . fromMaybe cide $ oalias
    enumDef enum hide trans (map identToString derive) pos
expandHook hook@(CHSCall isPure isUns (CHSRoot _ ide) oalias pos) _ =
  do
    traceEnter
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used
    -- afterwards instead of the original one
    --
    (ObjCO cdecl, ide') <- findFunObj ide True
    let ideLexeme = identToString ide'  -- orignl name might have been a shadow
        hsLexeme  = ideLexeme `maybe` identToString $ oalias
        cdecl'    = ide' `simplifyDecl` cdecl
    callImport hook isPure isUns ideLexeme hsLexeme cdecl' pos
    return hsLexeme
  where
    traceEnter = traceGenBind $
      "** Call hook for `" ++ identToString ide ++ "':\n"
expandHook hook@(CHSCall isPure isUns apath oalias pos) _ =
  do
    traceEnter

    (decl, offsets) <- accessPath apath
    ptrTy <- extractSimpleType False pos decl
    ty <- case ptrTy of
        PtrET f@(FunET _ _) -> return f
        _ -> funPtrExpectedErr pos

    traceValueType ty
    set_get <- setGet pos CHSGet offsets ptrTy

    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used
    -- afterwards instead of the original one
    --
    -- (ObjCO cdecl, ide) <- findFunObj ide True
    let ideLexeme = identToString $ apathToIdent apath
        hsLexeme  = ideLexeme `maybe` identToString $ oalias
        -- cdecl'    = ide `simplifyDecl` cdecl
        args      = concat [ " x" ++ show n | n <- [1..numArgs ty] ]

    callImportDyn hook isPure isUns ideLexeme hsLexeme decl ty pos
    return $ "(\\o" ++ args ++ " -> " ++ set_get ++ " o >>= \\f -> "
             ++ hsLexeme ++ " f" ++ args ++ ")"
  where
    traceEnter = traceGenBind $
      "** Indirect call hook for `" ++ identToString (apathToIdent apath) ++ "':\n"
    traceValueType et  = traceGenBind $
      "Type of accessed value: " ++ showExtType et ++ "\n"
expandHook (CHSFun isPure isUns (CHSRoot _ ide)
            oalias ctxt parms parm pos) hkpos =
  do
    traceEnter
    traceGenBind $ "ide = '" ++ show ide ++ "'\n"
    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used
    -- afterwards instead of the original one
    --
    (ObjCO cdecl, cide) <- findFunObj ide True
    let ideLexeme = identToString ide  -- orignal name might have been a shadow
        hsLexeme  = ideLexeme `maybe` identToString $ oalias
        fiLexeme  = hsLexeme ++ "'_"   -- Urgh - probably unqiue...
        fiIde     = internalIdent fiLexeme
        cdecl'    = cide `simplifyDecl` cdecl
        callHook  = CHSCall isPure isUns (CHSRoot False cide) (Just fiIde) pos
    callImport callHook isPure isUns (identToString cide) fiLexeme cdecl' pos

    extTy <- extractFunType pos cdecl' True
    funDef isPure hsLexeme fiLexeme extTy ctxt parms parm Nothing pos hkpos
  where
    traceEnter = traceGenBind $
      "** Fun hook for `" ++ identToString ide ++ "':\n"
expandHook (CHSFun isPure isUns apath oalias ctxt parms parm pos) hkpos =
  do
    traceEnter

    (decl, offsets) <- accessPath apath
    ptrTy <- extractSimpleType False pos decl
    ty <- case ptrTy of
        PtrET f@(FunET _ _) -> return f
        _ -> funPtrExpectedErr pos

    traceValueType ty

    -- get the corresponding C declaration; raises error if not found or not a
    -- function; we use shadow identifiers, so the returned identifier is used
    -- afterwards instead of the original one
    --
    -- (ObjCO cdecl, cide) <- findFunObj ide True
    let ideLexeme = identToString $ apathToIdent apath
        hsLexeme  = ideLexeme `maybe` identToString $ oalias
        fiLexeme  = hsLexeme ++ "'_"   -- Urgh - probably unqiue...
        fiIde     = internalIdent fiLexeme
        -- cdecl'    = cide `simplifyDecl` cdecl
        -- args      = concat [ " x" ++ show n | n <- [1..numArgs ty] ]
        callHook  = CHSCall isPure isUns apath (Just fiIde) pos
    callImportDyn callHook isPure isUns ideLexeme fiLexeme decl ty pos

    set_get <- setGet pos CHSGet offsets ptrTy
    funDef isPure hsLexeme fiLexeme (FunET ptrTy $ purify ty)
                  ctxt parms parm (Just set_get) pos hkpos
  where
    -- remove IO from the result type of a function ExtType.  necessary
    -- due to an unexpected interaction with the way funDef works
    purify (FunET a b) = FunET a (purify b)
    purify (IOET b)    = b
    purify a           = a

    traceEnter = traceGenBind $
      "** Fun hook for `" ++ identToString (apathToIdent apath) ++ "':\n"
    traceValueType et  = traceGenBind $
      "Type of accessed value: " ++ showExtType et ++ "\n"
expandHook (CHSField access path pos) _ =
  do
    traceInfoField
    traceGenBind $ "path = " ++ show path ++ "\n"
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
expandHook (CHSOffsetof path pos) _ =
  do
    traceGenBind $ "** offsetof hook:\n"
    (decl, offsets) <- accessPath path
    traceGenBind $ "Depth of access path: " ++ show (length offsets) ++ "\n"
    checkType decl offsets >>= \ offset -> return $ "(" ++ show offset ++ ")"
  where
    checkType decl [BitSize offset _] =
        extractCompType True True decl >>= \ compTy ->
        case compTy of
          ExtType et ->
            case et of
              (VarFunET  _) -> variadicErr pos pos
              (IOET      _) ->
                interr "GenBind.expandHook(CHSOffsetOf): Illegal type!"
              (UnitET     ) -> voidFieldErr pos
              (DefinedET _ _) -> return offset
              (PrimET (CUFieldPT _)) -> offsetBitfieldErr pos
              (PrimET (CSFieldPT _)) -> offsetBitfieldErr pos
              _             -> return offset
          SUType _ -> return offset
    checkType _ _ = offsetDerefErr pos
expandHook (CHSPointer isStar cName oalias ptrKind isNewtype oRefType emit
              pos) _ =
  do
    traceInfoPointer
    let hsIde  = fromMaybe cName oalias
        hsName = identToString hsIde
    hsIde `objIs` Pointer ptrKind isNewtype     -- register Haskell object
    --
    -- we check for a typedef declaration or tag (struct, union, or enum)
    --
    declOrTag <- lookupDeclOrTag cName True
    case declOrTag of
      Left cdecl -> do                          -- found a typedef declaration
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
                             traceInfoPtrType et
                             let et' = adjustPtr isStar et
                             when (isVariadic et')
                                  (variadicErr pos (posOf cDecl))
                             return (showExtType et', isFunExtType et')
            Just hsType -> return (identToString hsType, False)
            -- FIXME: it is not possible to determine whether `hsType'
            --   is a function; we would need to extend the syntax to
            --   allow `... -> fun HSTYPE' to explicitly mark function
            --   types if this ever becomes important
        traceInfoHsType hsName hsType
        pointerDef isStar cNameFull hsName ptrKind isNewtype hsType isFun emit
      Right tag -> do                           -- found a tag definition
        let cNameFull = tagName tag
        traceInfoCName "tag definition" cNameFull
        unless isStar $                         -- tags need an explicit `*'
          ptrExpectedErr (posOf cName)
        let hsType = case oRefType of
                       Nothing      -> "()"
                       Just hsType' -> identToString hsType'
        traceInfoHsType hsName hsType
        pointerDef isStar cNameFull hsName ptrKind isNewtype hsType False emit
  where
    -- remove a pointer level if the first argument is `False'
    --
    adjustPtr True  et                 = et
    adjustPtr False (PtrET et)         = et
    adjustPtr False (DefinedET _ _)    =
      interr "GenBind.adjustPtr: Can't adjust defined type"
    adjustPtr _     _                  =
      interr "GenBind.adjustPtr: Where is the Ptr?"
    --
    traceInfoPointer        = traceGenBind "** Pointer hook:\n"
    traceInfoPtrType et     = traceGenBind $
      "extracted ptr type is `" ++ showExtType et ++ "'\n"
    traceInfoHsType name ty = traceGenBind $
      "associated with Haskell entity `" ++ name ++ "'\nhaving type " ++ ty
      ++ "\n"
    traceInfoCName kind ide = traceGenBind $
      "found C " ++ kind ++ " for `" ++ identToString ide ++ "'\n"
expandHook (CHSClass oclassIde classIde typeIde pos) _ =
  do
    traceInfoClass
    classIde `objIs` Class oclassIde typeIde    -- register Haskell object
    superClasses <- collectClasses oclassIde
    Pointer ptrType isNewtype <- queryPointer typeIde
    when (ptrType == CHSStablePtr) $
      illegalStablePtrErr pos
    classDef pos (identToString classIde) (identToString typeIde)
             ptrType isNewtype superClasses
  where
    -- compile a list of all super classes (the direct super class first)
    --
    collectClasses            :: Maybe Ident -> GB [(String, String, HsObject)]
    collectClasses Nothing     = return []
    collectClasses (Just ide)  =
      do
        Class oclassIde' typeIde' <- queryClass ide
        ptr                       <- queryPointer typeIde'
        classes                   <- collectClasses oclassIde'
        return $ (identToString ide, identToString typeIde', ptr) : classes
    --
    traceInfoClass = traceGenBind $ "** Class hook:\n"

-- | produce code for an enumeration
--
-- * an extra instance declaration is required when any of the enumeration
--   constants is explicitly assigned a value in its definition
--
-- * the translation function strips prefixes where possible (different
--   enumerators maye have different prefixes)
--
enumDef :: CEnum -> String -> TransFun -> [String] -> Position -> GB String
enumDef (CEnum _ Nothing _ _) _ _ _ pos = undefEnumErr pos
enumDef (CEnum _ (Just list) _ _) hident trans userDerive _ =
  do
    (list', enumAuto) <- evalTagVals list
    let enumVals = fixTags [(trans ide, cexpr) | (ide, cexpr) <- list']
        defHead  = enumHead hident
        defBody  = enumBody (length defHead - 2) enumVals
        inst     = makeDerives
                   (if enumAuto then "Enum" : userDerive else userDerive) ++
                   "\n" ++
                   if enumAuto
                   then ""
                   else enumInst hident enumVals
    isEnum hident
    return $ defHead ++ defBody ++ inst
  where
    evalTagVals = liftM (second and . unzip) . mapM (uncurry evalTag)
    evalTag ide Nothing = return ((ide, Nothing), True)
    evalTag ide (Just exp) =  do
        val <- evalConstCExpr exp
        case val of
            IntResult v -> return ((ide, Just v), False)
            FloatResult _ -> illegalConstExprErr (posOf exp) "a float result"
    makeDerives [] = ""
    makeDerives dList = "\n  deriving (" ++ intercalate "," dList ++ ")"
    -- Fix implicit tag values
    fixTags = go 0
      where
        go _ [] = []
        go n  ((ide, exp):rest) =
            let val = case exp of
                    Nothing  -> n
                    Just m   -> m
            in (ide, val) : go (val+1) rest

-- | Haskell code for the head of an enumeration definition
--
enumHead       :: String -> String
enumHead ident  = "data " ++ ident ++ " = "

-- | Haskell code for the body of an enumeration definition
--
enumBody :: Int -> [(String, Integer)] -> String
enumBody indent ides  = constrs
  where
    constrs = intercalate separator . map fst $ sortBy (comparing snd) ides
    separator = "\n" ++ replicate indent ' ' ++ "| "

-- | Num instance for C Integers
-- We should preserve type flags and repr if possible
instance Num CInteger where
  fromInteger = cInteger
  (+) a b = cInteger (getCInteger a + getCInteger b)
  (*) a b = cInteger (getCInteger a * getCInteger b)
  abs a = cInteger (abs $ getCInteger a)
  signum a = cInteger (signum $ getCInteger a)
-- | Haskell code for an instance declaration for 'Enum'
--
-- * the expression of all explicitly specified tag values already have to be
--   in normal form, ie, to be an int constant
--
-- * enumerations start at 0 and whenever an explicit value is specified,
--   following tags are assigned values continuing from the explicitly
--   specified one
--
enumInst :: String -> [(String, Integer)] -> String
enumInst ident list' = intercalate "\n"
  [ "instance Enum " ++ ident ++ " where"
  , succDef
  , predDef
  , enumFromToDef
  , enumFromDef
  , fromDef
  , toDef
  ]
  where
    concatFor = flip concatMap
    -- List of _all values_ (including aliases) and their associated tags
    list   = sortBy (comparing snd) list'
    -- List of values without aliases and their associated tags
    toList = stripAliases list
    -- Generate explicit tags for all values:
    succDef = let idents = map fst toList
                  aliases = map (map fst) $ groupBy ((==) `on` snd) list
                  defs =  concat $ zipWith
                          (\is s -> concatFor is $ \i -> "  succ " ++ i
                                                         ++ " = " ++ s ++ "\n")
                          aliases
                          (tail idents)
                  lasts = concatFor (last aliases) $ \i ->
                              "  succ " ++ i ++ " = error \""
                                 ++ ident ++ ".succ: " ++ i ++
                                 " has no successor\"\n"
                  in defs ++ lasts
    predDef = let idents = map fst toList
                  aliases = map (map fst) $ groupBy ((==) `on` snd) list
                  defs =  concat $ zipWith
                          (\is s -> concatFor is $ \i -> "  pred " ++ i
                                                         ++ " = " ++ s ++ "\n")
                          (tail aliases)
                          idents
                  firsts = concatFor (head aliases) $ \i ->
                               "  pred " ++ i ++ " = error \""
                                 ++ ident ++ ".pred: " ++ i ++
                                 " has no predecessor\"\n"
                  in defs ++ firsts
    enumFromToDef = intercalate "\n"
                    [ "  enumFromTo from to = go from"
                    , "    where"
                    , "      end = fromEnum to"
                    , "      go v = case compare (fromEnum v) end of"
                    , "                 LT -> v : go (succ v)"
                    , "                 EQ -> [v]"
                    , "                 GT -> []"
                    , ""
                    ]
    enumFromDef = let lastIdent = fst $ last list
               in "  enumFrom from = enumFromTo from " ++ lastIdent ++ "\n"

    fromDef = concatFor list (\(ide, val) -> "  fromEnum " ++ ide ++ " = "
                               ++ show' val ++ "\n")

    toDef = (concatFor toList (\(ide, val) -> "  toEnum " ++ show' val ++ " = "
                                       ++ ide ++ "\n"))
            -- Default case:
            ++ "  toEnum unmatched = error (\"" ++ ident
               ++ ".toEnum: Cannot match \" ++ show unmatched)\n"
    show' x = if x < 0 then "(" ++ show x ++ ")" else show x
    stripAliases :: [(String, Integer)] -> [(String, Integer)]
    stripAliases = nubBy ((==) `on` snd)

-- | generate a foreign import declaration that is put into the delayed code
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
    header  <- getSwitch headerSB
    when (isVariadic extType) (variadicErr pos (posOf cdecl))
    delayCode hook (foreignImport (extractCallingConvention cdecl)
                    header ideLexeme hsLexeme isUns extType)
    traceFunType extType
  where
    traceFunType et = traceGenBind $
      "Imported function type: " ++ showExtType et ++ "\n"

callImportDyn :: CHSHook -> Bool -> Bool -> String -> String -> CDecl -> ExtType
              -> Position -> GB ()
callImportDyn hook _isPure isUns ideLexeme hsLexeme cdecl ty pos =
  do
    -- compute the external type from the declaration, and delay the foreign
    -- export declaration
    --
    when (isVariadic ty) (variadicErr pos (posOf cdecl))
    delayCode hook (foreignImportDyn (extractCallingConvention cdecl)
                    ideLexeme hsLexeme isUns ty)
    traceFunType ty
  where
    traceFunType et = traceGenBind $
      "Imported function type: " ++ showExtType et ++ "\n"

-- | Haskell code for the foreign import declaration needed by a call hook
--
foreignImport :: CallingConvention -> String -> String -> String -> Bool -> ExtType -> String
foreignImport cconv header ident hsIdent isUnsafe ty  =
  "foreign import " ++ showCallingConvention cconv ++ " " ++ safety
  ++ " " ++ show entity ++
  "\n  " ++ hsIdent ++ " :: " ++ showExtType ty ++ "\n"
  where
    safety = if isUnsafe then "unsafe" else "safe"
    entity | null header = ident
           | otherwise   = header ++ " " ++ ident

-- | Haskell code for the foreign import dynamic declaration needed by a call hook
--
foreignImportDyn :: CallingConvention -> String -> String -> Bool -> ExtType -> String
foreignImportDyn cconv _ident hsIdent isUnsafe ty  =
  "foreign import " ++ showCallingConvention cconv ++ " " ++ safety
    ++ " \"dynamic\"\n  " ++
    hsIdent ++ " :: FunPtr( " ++ showExtType ty ++ " ) -> " ++
    showExtType ty ++ "\n"
  where
    safety = if isUnsafe then "unsafe" else "safe"

-- | produce a Haskell function definition for a fun hook
--
-- * FIXME: There's an ugly special case in here: to support dynamic fun hooks
--   I had to add a special second marshaller for the first argument,
--   which, if present, is inserted just before the function call.  This
--   is probably not the most elegant solution, it's just the only one I
--   can up with at the moment.  If present, this special marshaller is
--   an io action (like 'peek' and unlike 'with'). -- US

funDef :: Bool               -- pure function?
       -> String             -- name of the new Haskell function
       -> String             -- Haskell name of the foreign imported C function
       -> ExtType            -- simplified declaration of the C function
       -> Maybe String       -- type context of the new Haskell function
       -> [CHSParm]          -- parameter marshalling description
       -> CHSParm            -- result marshalling description
       -> Maybe String       -- optional additional marshaller for first arg
       -> Position           -- source location of the hook
       -> Position           -- source location of the start of the hook
       -> GB String          -- Haskell code in text form
funDef isPure hsLexeme fiLexeme extTy octxt parms parm marsh2 pos hkpos =
  do
    (parms', parm', isImpure) <- addDftMarshaller pos parms parm extTy

    traceMarsh parms' parm' isImpure
    let
      sig       = hsLexeme ++ " :: " ++ funTy parms' parm' ++ "\n"
      marshs    = [marshArg i parm'' | (i, parm'') <- zip [1..] parms']
      funArgs   = [funArg   | (funArg, _, _, _, _)   <- marshs, funArg   /= ""]
      marshIns  = [marshIn  | (_, marshIn, _, _, _)  <- marshs]
      callArgs  = [callArg  | (_, _, cs, _, _)  <- marshs, callArg <- cs]
      marshOuts = [marshOut | (_, _, _, marshOut, _) <- marshs, marshOut /= ""]
      retArgs   = [retArg   | (_, _, _, _, retArg)   <- marshs, retArg   /= ""]
      funHead   = hsLexeme ++ join funArgs ++ " =\n" ++
                  if isPure && isImpure then "  unsafePerformIO $\n" else ""
      call      = if isPure
                  then "  let {res = " ++ fiLexeme ++ joinCallArgs ++ "} in\n"
                  else "  " ++ fiLexeme ++ joinCallArgs ++ case parm of
                    CHSParm _ "()" _ Nothing _ -> " >>\n"
                    _                        -> " >>= \\res ->\n"
      joinCallArgs = case marsh2 of
                        Nothing -> join callArgs
                        Just _  -> join ("b1'" : drop 1 callArgs)
      mkMarsh2  = case marsh2 of
                        Nothing -> ""
                        Just m  -> "  " ++ m ++ " " ++
                                   join (take 1 callArgs) ++
                                   " >>= \\b1' ->\n"
      marshRes  = case parm' of
                    CHSParm _ _ _twoCVal (Just (_     , CHSVoidArg  )) _ -> ""
                    CHSParm _ _ _twoCVal (Just (omBody, CHSIOVoidArg)) _ ->
                      "  " ++ marshBody omBody ++ " res >> \n"
                    CHSParm _ _ _twoCVal (Just (omBody, CHSIOArg     )) _ ->
                      "  " ++ marshBody omBody ++ " res >>= \\res' ->\n"
                    CHSParm _ _ _twoCVal (Just (omBody, CHSValArg    )) _ ->
                      "  let {res' = " ++ marshBody omBody ++ " res} in\n"
                    CHSParm _ _ _       Nothing                    _ ->
                      interr "GenBind.funDef: marshRes: no default?"

      marshBody (Left ide) = identToString ide
      marshBody (Right str) = "(" ++ str ++ ")"

      retArgs'  = case parm' of
                    CHSParm _ _ _ (Just (_, CHSVoidArg))   _ ->        retArgs
                    CHSParm _ _ _ (Just (_, CHSIOVoidArg)) _ ->        retArgs
                    _                                        -> "res'":retArgs
      ret       = "(" ++ concat (intersperse ", " retArgs') ++ ")"
      funBody   = joinLines marshIns  ++
                  mkMarsh2            ++
                  call                ++
                  marshRes            ++
                  joinLines marshOuts ++
                  "  " ++
                  (if isImpure || not isPure then "return " else "") ++ ret

      pad code = let padding = replicate (posColumn hkpos - 1) ' '
                     (l:ls) = lines code
                 in unlines $ l : map (padding ++) ls

    return $ pad $ sig ++ funHead ++ funBody
  where
    join      = concatMap (' ':)
    joinLines = concatMap (\s -> "  " ++ s ++ "\n")
    --
    -- construct the function type
    --
    -- * specified types appear in the argument and result only if their "in"
    --   and "out" marshaller, respectively, is not the `void' marshaller
    --
    funTy parms' parm' =
      let
        ctxt   = case octxt of
                   Nothing      -> ""
                   Just ctxtStr -> ctxtStr ++ " => "
        argTys = ["(" ++ ty ++ ")" | CHSParm im ty _ _  _ <- parms'      , notVoid im]
        resTys = ["(" ++ ty ++ ")" | CHSParm _  ty _ om _ <- parm':parms', notVoid om]
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
        notVoid (Just (_, kind)) = kind /= CHSVoidArg && kind /= CHSIOVoidArg
    --
    -- for an argument marshaller, generate all "in" and "out" marshalling
    -- code fragments
    --
    marshArg i (CHSParm (Just (imBody, imArgKind)) _ twoCVal
                        (Just (omBody, omArgKind)) _        ) =
      let
        a        = "a" ++ show (i :: Int)
        imStr    = marshBody imBody
        imApp    = imStr ++ " " ++ a
        funArg   = if imArgKind == CHSVoidArg then "" else a
        inBndr   = if twoCVal
                     then "(" ++ a ++ "'1, " ++ a ++ "'2)"
                     else a ++ "'"
        marshIn  = case imArgKind of
                     CHSVoidArg -> imStr ++ " $ \\" ++ inBndr ++ " -> "
                     CHSIOArg   -> imApp ++ " $ \\" ++ inBndr ++ " -> "
                     CHSValArg  -> "let {" ++ inBndr ++ " = " ++
                                   imApp ++ "} in "
        callArgs = if twoCVal
                     then [a ++ "'1 ", a ++ "'2"]
                     else [a ++ "'"]
        omApp    = marshBody omBody ++ " " ++ join callArgs
        outBndr  = a ++ "''"
        marshOut = case omArgKind of
                     CHSVoidArg   -> ""
                     CHSIOVoidArg -> omApp ++ ">>"
                     CHSIOArg     -> omApp ++ ">>= \\" ++ outBndr ++ " -> "
                     CHSValArg    -> "let {" ++ outBndr ++ " = " ++
                                   omApp ++ "} in "
        retArg   = if omArgKind == CHSVoidArg || omArgKind == CHSIOVoidArg then "" else outBndr

        marshBody (Left ide) = identToString ide
        marshBody (Right str) = "(" ++ str ++ ")"
      in
      (funArg, marshIn, callArgs, marshOut, retArg)
    marshArg _ _ = interr "GenBind.funDef: Missing default?"
    --
    traceMarsh parms' parm' isImpure = traceGenBind $
      "Marshalling specification including defaults: \n" ++
      showParms (parms' ++ [parm']) "" ++
      "  The marshalling is " ++ if isImpure then "impure.\n" else "pure.\n"
      where
        showParms []               = id
        showParms (parm'':parms'') = showString "  "
                                     . showCHSParm parm''
                                     . showChar '\n'
                                     . showParms parms''

-- | add default marshallers for "in" and "out" marshalling
--
addDftMarshaller :: Position -> [CHSParm] -> CHSParm -> ExtType
                 -> GB ([CHSParm], CHSParm, Bool)
addDftMarshaller pos parms parm extTy = do
  let (resTy, argTys)  = splitFunTy extTy
  (parm' , isImpure1) <- checkResMarsh parm resTy
  (parms', isImpure2) <- addDft parms argTys
  return (parms', parm', isImpure1 || isImpure2)
  where
    -- the result marshalling may not use an "in" marshaller and can only have
    -- one C value
    --
    -- * a default marshaller maybe used for "out" marshalling
    --
    checkResMarsh (CHSParm (Just _) _  _    _       pos') _   =
      resMarshIllegalInErr      pos'
    checkResMarsh (CHSParm _        _  True _       pos') _   =
      resMarshIllegalTwoCValErr pos'
    checkResMarsh (CHSParm _        ty _    omMarsh pos') cTy = do
      (imMarsh', _       ) <- addDftVoid Nothing
      (omMarsh', isImpure) <- addDftOut pos' omMarsh ty [cTy]
      return (CHSParm imMarsh' ty False omMarsh' pos', isImpure)
    --
    splitFunTy (FunET UnitET ty ) = splitFunTy ty
    splitFunTy (FunET ty1    ty2) = let
                                      (resTy, argTys) = splitFunTy ty2
                                    in
                                    (resTy, ty1:argTys)
    splitFunTy resTy              = (resTy, [])
    --
    -- match Haskell with C arguments (and results)
    --
    addDft ((CHSParm imMarsh hsTy False omMarsh p):parms'') (cTy    :cTys) = do
      (imMarsh', isImpureIn ) <- addDftIn   p imMarsh hsTy [cTy]
      (omMarsh', isImpureOut) <- addDftVoid    omMarsh
      (parms'  , isImpure   ) <- addDft parms'' cTys
      return (CHSParm imMarsh' hsTy False omMarsh' p : parms',
              isImpure || isImpureIn || isImpureOut)
    addDft ((CHSParm imMarsh hsTy True  omMarsh p):parms'') (cTy1:cTy2:cTys) =
      do
      (imMarsh', isImpureIn ) <- addDftIn   p imMarsh hsTy [cTy1, cTy2]
      (omMarsh', isImpureOut) <- addDftVoid   omMarsh
      (parms'  , isImpure   ) <- addDft parms'' cTys
      return (CHSParm imMarsh' hsTy True omMarsh' p : parms',
              isImpure || isImpureIn || isImpureOut)
    addDft []                                             []               =
      return ([], False)
    addDft ((CHSParm _       _    _     _     pos'):_)    []               =
      marshArgMismatchErr pos' "This parameter is in excess of the C arguments."
    addDft []                                             (_:_)            =
      marshArgMismatchErr pos "Parameter marshallers are missing."
    --
    addDftIn _    imMarsh@(Just (_, kind)) _    _    = return (imMarsh,
                                                              kind == CHSIOArg)
    addDftIn pos'  _imMarsh@Nothing        hsTy cTys = do
      marsh <- lookupDftMarshIn hsTy cTys
      when (isNothing marsh) $
        noDftMarshErr pos' "\"in\"" hsTy cTys
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    addDftOut _    omMarsh@(Just (_, kind)) _    _    = return (omMarsh,
                                                              kind == CHSIOArg)
    addDftOut pos' _omMarsh@Nothing         hsTy cTys = do
      marsh <- lookupDftMarshOut hsTy cTys
      when (isNothing marsh) $
        noDftMarshErr pos' "\"out\"" hsTy cTys
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    -- add void marshaller if no explict one is given
    --
    addDftVoid marsh@(Just (_, kind)) = return (marsh, kind == CHSIOArg)
    addDftVoid        Nothing         = do
      return (Just (Left (internalIdent "void"), CHSVoidArg), False)

-- | compute from an access path, the declarator finally accessed and the index
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
accessPath (CHSRoot _ ide) =                            -- t
  do
    decl <- findAndChaseDecl ide False True
    return (ide `simplifyDecl` decl, [BitSize 0 0])
accessPath (CHSDeref (CHSRoot _ ide) _) =               -- *t
  do
    decl <- findAndChaseDecl ide True True
    return (ide `simplifyDecl` decl, [BitSize 0 0])
accessPath (CHSRef (CHSRoot str ide1) ide2) =           -- t.m
  do
    su <- lookupStructUnion ide1 str True
    (offset, decl') <- refStruct su ide2
    adecl <- replaceByAlias decl'
    return (adecl, [offset])
accessPath (CHSRef (CHSDeref (CHSRoot str ide1) _) ide2) =  -- t->m
  do
    su <- lookupStructUnion ide1 str True
    (offset, decl') <- refStruct su ide2
    adecl <- replaceByAlias decl'
    return (adecl, [offset])
accessPath (CHSRef path ide) =                          -- a.m
  do
    (decl, offset:offsets) <- accessPath path
    assertPrimDeclr ide decl
    su <- structFromDecl (posOf ide) decl
    (addOffset, decl') <- refStruct su ide
    adecl <- replaceByAlias decl'
    return (adecl, offset `addBitSize` addOffset : offsets)
  where
    assertPrimDeclr ide' (CDecl _ [declr] _) =
      case declr of
        (Just (CDeclr _ [] _ _ _), _, _) -> return ()
        _                                -> structExpectedErr ide'
accessPath (CHSDeref path _pos) =                        -- *a
  do
    (decl, offsets) <- accessPath path
    decl' <- derefOrErr decl
    adecl  <- replaceByAlias decl'
    return (adecl, BitSize 0 0 : offsets)
  where
    derefOrErr (CDecl specs [(Just declr, oinit, oexpr)] at) =
      do
        declr' <- derefDeclr declr
        return $ CDecl specs [(Just declr', oinit, oexpr)] at
    derefDeclr (CDeclr oid (CPtrDeclr _ _: derived') asm ats n) = return $ CDeclr oid derived' asm ats n
    derefDeclr (CDeclr _oid _unexp_deriv _ _ n) = ptrExpectedErr (posOf n)

-- | replaces a decleration by its alias if any
--
-- * the alias inherits any field size specification that the original
--   declaration may have
--
-- * declaration must have exactly one declarator
--
replaceByAlias                                :: CDecl -> GB CDecl
replaceByAlias cdecl@(CDecl _ [(_, _, size)] _at)  =
  do
    ocdecl <- checkForAlias cdecl
    case ocdecl of
      Nothing                                  -> return cdecl
      Just (CDecl specs [(declr, init', _)] at) ->   -- form of an alias
        return $ CDecl specs [(declr, init', size)] at

-- | given a structure declaration and member name, compute the offset of the
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

-- | does the given declarator define the given name?
--
declNamed :: CDecl -> Ident -> Bool
(CDecl _ [(Nothing   , _, _)] _) `declNamed` _   = False
(CDecl _ [(Just declr, _, _)] _) `declNamed` ide = declr `declrNamed` ide
(CDecl _ []                   _) `declNamed` _   =
  interr "GenBind.declNamed: Abstract declarator in structure!"
_                                `declNamed` _   =
  interr "GenBind.declNamed: More than one declarator!"

-- | Haskell code for writing to or reading from a struct
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
          Nothing      -> return $ case access of       -- not a bitfield
                            CHSGet -> peekOp offset tyTag
                            CHSSet -> pokeOp offset tyTag "val"
--FIXME: must take `bitfieldDirection' into account
          Just (_, bs) -> return $ case access of       -- a bitfield
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
              bitsPerField    = show $ CInfo.size CIntPT * 8
              --
              insertBitfield  = "; let {val' = (org .&. " ++ middleMask
                                ++ ") .|. (val `shiftL` "
                                ++ show bitOffset ++ ")}; "
              middleMask      = "fromIntegral (((maxBound::CUInt) `shiftL` "
                                ++ show bs ++ ") `rotateL` "
                                ++ show bitOffset ++ ")"
    setGetBody (BitSize offset 0 : offsetsrem) =
      do
        code <- setGetBody offsetsrem
        return $ "ptr <- peekByteOff ptr " ++ show offset ++ "; " ++ code
    setGetBody (BitSize _      _ : _      ) =
      derefBitfieldErr pos
    --
    -- check that the type can be marshalled and compute extra operations for
    -- bitfields
    --
    checkType (VarFunET  _    )          = variadicErr pos pos
    checkType (IOET      _    )          = interr "GenBind.setGet: Illegal \
                                                  \type!"
    checkType (UnitET         )          = voidFieldErr pos
    checkType (DefinedET _ _  )          = return Nothing-- can't check further
    checkType (PrimET    (CUFieldPT bs)) = return $ Just (False, bs)
    checkType (PrimET    (CSFieldPT bs)) = return $ Just (True , bs)
    checkType _                          = return Nothing
    --
    peekOp off tyTag     = "peekByteOff ptr " ++ show off ++ " ::IO " ++ tyTag
    pokeOp off tyTag var = "pokeByteOff ptr " ++ show off ++ " (" ++ var
                           ++ "::" ++ tyTag ++ ")"

-- | generate the type definition for a pointer hook and enter the required type
-- mapping into the 'ptrmap'
--
pointerDef :: Bool              -- explicit `*' in pointer hook
           -> Ident             -- full C name
           -> String            -- Haskell name
           -> CHSPtrType        -- kind of the pointer
           -> Bool              -- explicit newtype tag
           -> String            -- Haskell type expression of pointer argument
           -> Bool              -- do we have a pointer to a function?
           -> Bool              -- shall we emit code?
           -> GB String
pointerDef isStar cNameFull hsName ptrKind isNewtype hsType isFun emit =
  do
    let ptrArg  = if isNewtype
                  then hsName           -- abstract type
                  else hsType           -- concrete type
        ptrCon  = case ptrKind of
                    CHSPtr | isFun -> "FunPtr"
                    _              -> show ptrKind
        ptrType = ptrCon ++ " (" ++ ptrArg ++ ")"
        thePtr  = (isStar, cNameFull)
    case ptrKind of
      CHSForeignPtr -> thePtr `ptrMapsTo` ("Ptr (" ++ ptrArg ++ ")",
                                           "Ptr (" ++ ptrArg ++ ")")
      _             -> thePtr `ptrMapsTo` (hsName, hsName)
    return $
      case (emit, isNewtype) of
        (False, _)     -> ""    -- suppress code generation
        (True , True)  ->
          "newtype " ++ hsName ++ " = " ++ hsName ++ " (" ++ ptrType ++ ")" ++
           withForeignFun
        (True , False) ->
          "type "    ++ hsName ++ " = "                   ++ ptrType
    where
      -- if we have a foreign pointer wrapped into a newtype, provide a
      -- safe unwrapping function automatically
      --
      withForeignFun
        | ptrKind == CHSForeignPtr =
          "\nwith" ++ hsName ++ " :: " ++
          hsName ++ " -> (Ptr " ++ hsName ++ " -> IO b) -> IO b" ++
          "\nwith" ++ hsName ++ " (" ++ hsName ++ " fptr) = withForeignPtr fptr"
        | otherwise                = ""

-- | generate the class and instance definitions for a class hook
--
-- * the pointer type must not be a stable pointer
--
-- * the first super class (if present) must be the direct superclass
--
-- * all Haskell objects in the superclass list must be pointer objects
--
classDef :: Position                     -- for error messages
         -> String                       -- class name
         -> String                       -- pointer type name
         -> CHSPtrType                   -- type of the pointer
         -> Bool                         -- is a newtype?
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
      classDefStr     =
        "class " ++ classDefContext ++ className ++ " p where\n"
        ++ "  " ++ toMethodName   ++ " :: p -> " ++ typeName ++ "\n"
        ++ "  " ++ fromMethodName ++ " :: " ++ typeName ++ " -> p\n"
      instDef         =
        "instance " ++ className ++ " " ++ typeName ++ " where\n"
        ++ "  " ++ toMethodName   ++ " = id\n"
        ++ "  " ++ fromMethodName ++ " = id\n"
    instDefs <- castInstDefs superClasses
    return $ classDefStr ++ instDefs ++ instDef
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
            castFun         = "cast" ++ show ptrType
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

-- | the result of a constant expression
--
data ConstResult = IntResult   Integer
                 | FloatResult Float

-- | types that may occur in foreign declarations, ie, Haskell land types
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
data ExtType = FunET     ExtType ExtType        -- function
             | IOET      ExtType                -- operation with side effect
             | PtrET     ExtType                -- typed pointer
             | DefinedET CDecl String           -- aliased type
             | PrimET    CPrimType              -- basic C type
             | UnitET                           -- void
             | VarFunET  ExtType                -- variadic function

instance Eq ExtType where
  (FunET     t1 t2) == (FunET     t1' t2') = t1 == t1' && t2 == t2'
  (IOET      t    ) == (IOET      t'     ) = t == t'
  (PtrET     t    ) == (PtrET     t'     ) = t == t'
  (DefinedET _  s ) == (DefinedET _   s' ) = s == s'
  (PrimET    t    ) == (PrimET    t'     ) = t == t'
  (VarFunET  t    ) == (VarFunET  t'     ) = t == t'
  UnitET            == UnitET              = True

-- | composite C type
--
data CompType = ExtType  ExtType                -- external type
              | SUType   CStructUnion           -- structure or union

-- | check whether an external type denotes a function type
--
isFunExtType             :: ExtType -> Bool
isFunExtType (FunET    _ _) = True
isFunExtType (VarFunET _  ) = True
isFunExtType (IOET     _  ) = True
isFunExtType _              = False

numArgs                  :: ExtType -> Int
numArgs (FunET _ f) = 1 + numArgs f
numArgs _           = 0

-- | pretty print an external type
--
-- * a previous version of this function attempted to not print unnecessary
--   brackets; this however doesn't work consistently due to `DefinedET'; so,
--   we give up on the idea (preferring simplicity)
--
showExtType                        :: ExtType -> String
showExtType (FunET UnitET res)      = showExtType res
showExtType (FunET arg res)         = "(" ++ showExtType arg ++ " -> "
                                      ++ showExtType res ++ ")"
showExtType (VarFunET res)          = "( ... -> " ++ showExtType res ++ ")"
showExtType (IOET t)                = "(IO " ++ showExtType t ++ ")"
showExtType (PtrET t)               = let ptrCon = if isFunExtType t
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
showExtType (PrimET (CSFieldPT bs)) = "CInt{-:" ++ show bs ++ "-}"
showExtType (PrimET (CUFieldPT bs)) = "CUInt{-:" ++ show bs ++ "-}"
showExtType UnitET                  = "()"

-- | compute the type of the C function declared by the given C object
--
-- * the identifier specifies in which of the declarators we are interested
--
-- * if the third argument is 'True', the function result should not be
--   wrapped into an 'IO' type
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
    preResultType <- extractSimpleType True pos resultDecl
    --
    -- we can now add the 'IO' monad if this is no pure function
    --
    let protoResultType = if isPure
                          then      preResultType
                          else IOET preResultType
    let resultType = if variadic
                     then VarFunET protoResultType
                     else          protoResultType
    --
    -- compute function arguments and create a function type (a function
    -- prototype with `void' as its single argument declares a nullary
    -- function)
    --
    argTypes <- mapM (extractSimpleType False pos) args
    return $ foldr FunET resultType argTypes

-- | compute a non-struct/union type from the given declaration
--
-- * the declaration may have at most one declarator
--
extractSimpleType                    :: Bool -> Position -> CDecl -> GB ExtType
extractSimpleType isResult pos cdecl  =
  do
    traceEnter
    ct <- extractCompType isResult True cdecl
    case ct of
      ExtType et -> return et
      SUType  _  -> illegalStructUnionErr (posOf cdecl) pos
  where
    traceEnter = traceGenBind $
      "Entering `extractSimpleType' (" ++ (if isResult then "" else "not ")
      ++ "for a result)...\n"

-- | compute a Haskell type for a type referenced in a C pointer type
--
-- * the declaration may have at most one declarator
--
-- * unknown struct/union types are mapped to '()'
--
-- * do *not* take aliases into account
--
-- * NB: this is by definition not a result type
--
extractPtrType :: CDecl -> GB ExtType
extractPtrType cdecl = do
  ct <- extractCompType False False cdecl
  case ct of
    ExtType et -> return et
    SUType  _  -> return UnitET

-- | compute a Haskell type from the given C declaration, where C functions are
-- represented by function pointers
--
-- * the declaration may have at most one declarator
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
--                   for pointer types; if this ever changes, we need to
--                   handle `DefinedET's differently.  The problem is that
--                   entries in the pointer map currently prevent
--                   `extractCompType' from looking further "into" the
--                   definition of that pointer.
--
extractCompType :: Bool -> Bool -> CDecl -> GB CompType
extractCompType isResult usePtrAliases cdecl@(CDecl specs' declrs ats)  =
  if length declrs > 1
  then interr "GenBind.extractCompType: Too many declarators!"
  else case declrs of
    [(Just declr, _, size)] | isPtrDeclr declr -> ptrType declr
                            | isFunDeclr declr -> funType
                            | otherwise        -> aliasOrSpecType size
    []                                         -> aliasOrSpecType Nothing
  where
    -- handle explicit pointer types
    --
    ptrType declr = do
      tracePtrType
      let declrs' = dropPtrDeclr declr          -- remove indirection
          cdecl'  = CDecl specs' [(Just declrs', Nothing, Nothing)] ats
          oalias  = checkForOneAliasName cdecl' -- is only an alias remaining?
          osu     = checkForOneCUName cdecl'
          oname   = if oalias == Nothing then osu else oalias
      oHsRepr <- case oname of
                   Nothing  -> return $ Nothing
                   Just ide -> queryPtr (True, ide)
      case oHsRepr of
        Just repr | usePtrAliases  -> ptrAlias repr     -- got an alias
        _                          -> do                -- no alias => recurs
          ct <- extractCompType False usePtrAliases cdecl'
          returnX $ case ct of
                      ExtType et -> PtrET et
                      SUType  _  -> PtrET UnitET
    --
    -- handle explicit function types
    --
    -- FIXME: we currently regard any functions as being impure (ie, being IO
    --        functions); is this ever going to be a problem?
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
        Nothing   -> specType (posOf cdecl) specs' size
        Just ide  -> do                    -- this is a typedef alias
          traceAlias ide
          oHsRepr <- queryPtr (False, ide) -- check for pointer hook alias
          case oHsRepr of
            Just repr | usePtrAliases
               -> ptrAlias repr    -- found a pointer hook alias
            _  -> do               -- skip current alias (only one)
                    cdecl' <- getDeclOf ide
                    let CDecl specs [(declr, init', _)] at =
                          ide `simplifyDecl` cdecl'
                        sdecl = CDecl specs [(declr, init', size)] at
                        -- propagate `size' down (slightly kludgy)
                    extractCompType isResult usePtrAliases sdecl
    --
    -- compute the result for a pointer alias
    --
    ptrAlias (repr1, repr2) =
      returnX $ DefinedET cdecl (if isResult then repr2 else repr1)
    --
    -- wrap an `ExtType' into a `CompType'
    --
    returnX retval            = return $ ExtType retval
    --
    tracePtrType = traceGenBind $ "extractCompType: explicit pointer type\n"
    traceFunType = traceGenBind $ "extractCompType: explicit function type\n"
    traceAliasOrSpecType Nothing  = traceGenBind $
      "extractCompType: checking for alias\n"
    traceAliasOrSpecType (Just _) = traceGenBind $
      "extractCompType: checking for alias of bitfield\n"
    traceAlias ide = traceGenBind $
      "extractCompType: found an alias called `" ++ identToString ide ++ "'\n"

-- | C to Haskell type mapping described in the DOCU section
--
typeMap :: [([CTypeSpec], ExtType)]
typeMap  = [([void]                      , UnitET           ),
            ([char]                      , PrimET CCharPT   ),
            ([unsigned, char]            , PrimET CUCharPT  ),
            ([signed, char]              , PrimET CSCharPT  ),
            ([signed]                    , PrimET CIntPT    ),
            ([int]                       , PrimET CIntPT    ),
            ([signed, int]               , PrimET CIntPT    ),
            ([short]                     , PrimET CShortPT  ),
            ([short, int]                , PrimET CShortPT  ),
            ([signed, short]             , PrimET CShortPT  ),
            ([signed, short, int]        , PrimET CShortPT  ),
            ([long]                      , PrimET CLongPT   ),
            ([long, int]                 , PrimET CLongPT   ),
            ([signed, long]              , PrimET CLongPT   ),
            ([signed, long, int]         , PrimET CLongPT   ),
            ([long, long]                , PrimET CLLongPT  ),
            ([long, long, int]           , PrimET CLLongPT  ),
            ([signed, long, long]        , PrimET CLLongPT  ),
            ([signed, long, long, int]   , PrimET CLLongPT  ),
            ([unsigned]                  , PrimET CUIntPT   ),
            ([unsigned, int]             , PrimET CUIntPT   ),
            ([unsigned, short]           , PrimET CUShortPT ),
            ([unsigned, short, int]      , PrimET CUShortPT ),
            ([unsigned, long]            , PrimET CULongPT  ),
            ([unsigned, long, int]       , PrimET CULongPT  ),
            ([unsigned, long, long]      , PrimET CULLongPT ),
            ([unsigned, long, long, int] , PrimET CULLongPT ),
            ([float]                     , PrimET CFloatPT  ),
            ([double]                    , PrimET CDoublePT ),
            ([long, double]              , PrimET CLDoublePT),
            ([enum]                      , PrimET CIntPT    )]
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

-- | compute the complex (external) type determined by a list of type specifiers
--
-- * may not be called for a specifier that defines a typedef alias
--
specType :: Position -> [CDeclSpec] -> Maybe CExpr -> GB CompType
specType cpos specs'' osize =
  let tspecs = [ts | CTypeSpec ts <- specs'']
  in case lookupTSpec tspecs typeMap of
    Just et | isUnsupportedType et -> unsupportedTypeSpecErr cpos
            | isNothing osize      -> return $ ExtType et     -- not a bitfield
            | otherwise            -> bitfieldSpec tspecs et osize  -- bitfield
    Nothing                        ->
      case tspecs of
        [CSUType   cu _] -> return $ SUType cu               -- struct or union
        [CEnumType _  _] -> return $ ExtType (PrimET CIntPT) -- enum
        [CTypeDef  _  _] -> interr "GenBind.specType: Illegal typedef alias!"
        _                -> illegalTypeSpecErr cpos
  where
    lookupTSpec = lookupBy matches
    --
    -- can't be a bitfield (yet)
    isUnsupportedType (PrimET et) = CInfo.size et == 0
    isUnsupportedType _           = False
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
      | otherwise                 = False
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
    eqSpec _               _               = False
    --
    bitfieldSpec :: [CTypeSpec] -> ExtType -> Maybe CExpr -> GB CompType
    bitfieldSpec tspecs et (Just sizeExpr) =  -- never called with 'Nothing'
      do
        PlatformSpec {bitfieldIntSignedPS = bitfieldIntSigned} <- getPlatform
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
              _                                   -> illegalFieldSizeErr pos
            where
              returnCT = return . ExtType . PrimET
              --
              int    = CIntType    undefined
              signed = CSignedType undefined

-- handle calling convention
-- -------------------------

data CallingConvention = StdCallConv
                       | CCallConv
                       deriving (Eq)

-- | determine the calling convention for the provided decl
extractCallingConvention :: CDecl -> CallingConvention
extractCallingConvention cdecl
  | hasStdCallAttr cdecl = StdCallConv
  | otherwise            = CCallConv
  where
    isStdCallAttr (CAttr x _ _) = identToString x == "stdcall"
                               || identToString x == "__stdcall__"

    hasStdCallAttr = any isStdCallAttr . funAttrs

    funAttrs (CDecl specs declrs _) =
      let (_,attrs',_,_,_) = partitionDeclSpecs specs
       in attrs' ++ funEndAttrs declrs ++ funPtrAttrs declrs

    -- attrs after the function name, e.g. void foo() __attribute__((...));
    funEndAttrs [(Just ((CDeclr _ (CFunDeclr _ _ _ : _) _ attrs _)), _, _)] = attrs
    funEndAttrs _                                                           = []

    -- attrs appearing within the declarator of a function pointer. As an
    -- example:
    -- typedef int (__stdcall *fp)();
    funPtrAttrs [(Just ((CDeclr _ (CPtrDeclr _ _ : CFunDeclr _ attrs _ : _) _ _ _)), _, _)] = attrs
    funPtrAttrs _ = []


-- | generate the necessary parameter for "foreign import" for the
-- provided calling convention
showCallingConvention :: CallingConvention -> String
showCallingConvention StdCallConv = "stdcall"
showCallingConvention CCallConv   = "ccall"


-- offset and size computations
-- ----------------------------

-- | precise size representation
--
-- * this is a pair of a number of octets and a number of bits
--
-- * if the number of bits is nonzero, the octet component is aligned by the
--   alignment constraint for 'CIntPT' (important for accessing bitfields with
--   more than 8 bits)
--
data BitSize = BitSize Int Int
             deriving (Eq, Show)

-- | ordering relation compares in terms of required storage units
--
instance Ord BitSize where
  bs1@(BitSize o1 b1) <  bs2@(BitSize o2 b2) =
    padBits bs1 < padBits bs2 || (o1 == o2 && b1 < b2)
  bs1                 <= bs2                 = bs1 < bs2 || bs1 == bs2
    -- the <= instance is needed for Ord's compare functions, which is used in
    -- the defaults for all other members

-- | add two bit size values
--
addBitSize                                 :: BitSize -> BitSize -> BitSize
addBitSize (BitSize o1 b1) (BitSize o2 b2)  = BitSize (o1 + o2 + overflow * CInfo.size CIntPT) rest
  where
    bitsPerBitfield  = CInfo.size CIntPT * 8
    (overflow, rest) = (b1 + b2) `divMod` bitsPerBitfield

-- | multiply a bit size by a constant (gives size of an array)
--
-- * not sure if this makes sense if the number of bits is non-zero.
--
scaleBitSize                  :: Int -> BitSize -> BitSize
scaleBitSize n (BitSize o1 b1) = BitSize (n * o1 + overflow) rest
  where
    bitsPerBitfield  = CInfo.size CIntPT * 8
    (overflow, rest) = (n * b1) `divMod` bitsPerBitfield

-- | pad any storage unit that is partially used by a bitfield
--
padBits               :: BitSize -> Int
padBits (BitSize o 0)  = o
padBits (BitSize o _)  = o + CInfo.size CIntPT

-- | compute the offset of the declarator in the second argument when it is
-- preceded by the declarators in the first argument
--
offsetInStruct                :: [CDecl] -> CDecl -> CStructTag -> GB BitSize
offsetInStruct []    _    _    = return $ BitSize 0 0
offsetInStruct decls decl tag  =
  do
    PlatformSpec {bitfieldAlignmentPS = bitfieldAlignment} <- getPlatform
    (offset, _) <- sizeAlignOfStruct decls tag
    (_, align)  <- sizeAlignOf decl
    return $ alignOffset offset align bitfieldAlignment

-- | compute the size and alignment (no padding at the end) of a set of
-- declarators from a struct
--
sizeAlignOfStruct :: [CDecl] -> CStructTag -> GB (BitSize, Int)
sizeAlignOfStruct []    _           = return (BitSize 0 0, 1)
sizeAlignOfStruct decls CStructTag  =
  do
    PlatformSpec {bitfieldAlignmentPS = bitfieldAlignment} <- getPlatform
    (offset, preAlign) <- sizeAlignOfStruct (init decls) CStructTag
    (size, align)      <- sizeAlignOf       (last decls)
    let sizeOfStruct  = alignOffset offset align bitfieldAlignment
                        `addBitSize` size
        align'        = if align > 0 then align else bitfieldAlignment
        alignOfStruct = preAlign `max` align'
    return (sizeOfStruct, alignOfStruct)

sizeAlignOfStruct decls CUnionTag   =
  do
    PlatformSpec {bitfieldAlignmentPS = bitfieldAlignment} <- getPlatform
    (sizes, aligns) <- mapAndUnzipM sizeAlignOf decls
    let aligns' = [if align > 0 then align else bitfieldAlignment
                  | align <- aligns]
    return (maximum sizes, maximum aligns')

-- | compute the size and alignment of the declarators forming a struct
-- including any end-of-struct padding that is needed to make the struct ``tile
-- in an array'' (K&R A7.4.8)
--
sizeAlignOfStructPad :: [CDecl] -> CStructTag -> GB (BitSize, Int)
sizeAlignOfStructPad decls tag =
  do
    PlatformSpec {bitfieldAlignmentPS = bitfieldAlignment} <- getPlatform
    (size, align) <- sizeAlignOfStruct decls tag
    return (alignOffset size align bitfieldAlignment, align)

-- | compute the size and alignment constraint of a given C declaration
--
sizeAlignOf       :: CDecl -> GB (BitSize, Int)
sizeAlignOfSingle :: CDecl -> GB (BitSize, Int)
--
-- * we make use of the assertion that 'extractCompType' can only return a
--   'DefinedET' when the declaration is a pointer declaration
-- * for arrays, alignment is the same as for the base type and the size
--   is the size of the base type multiplied by the number of elements.
--   FIXME: I'm not sure whether anything of this is guaranteed by ISO C
--   and I have no idea what happens when an array-of-bitfield is
--   declared.  At this time I don't care.  -- U.S. 05/2006
--
sizeAlignOf (CDecl dclspec
                   [(Just (CDeclr oide (CArrDeclr _ (CArrSize _ lexpr) _ : derived') _asm _ats n), init', expr)]
                   attr) =
  do
    (bitsize, align) <- sizeAlignOf (CDecl dclspec
                                           [(Just (CDeclr oide derived' Nothing [] n), init', expr)]
                                           attr)
    IntResult len <- evalConstCExpr lexpr
    return (fromIntegral len `scaleBitSize` bitsize, align)
sizeAlignOf (CDecl _ [(Just (CDeclr _ (CArrDeclr _ (CNoArrSize _) _ : _) _ _ _), _init, _expr)] _) =
    interr "GenBind.sizeAlignOf: array of undeclared size."
sizeAlignOf cdecl =
    sizeAlignOfSingle cdecl


sizeAlignOfSingle cdecl  =
  do
    ct <- extractCompType False False cdecl
    case ct of
      ExtType (FunET _ _        ) -> do
                                       align <- alignment CFunPtrPT
                                       return (bitSize CFunPtrPT, align)
      ExtType (VarFunET _       ) -> do
                                       align <- alignment CFunPtrPT
                                       return (bitSize CFunPtrPT, align)
      ExtType (IOET  _          ) -> interr "GenBind.sizeof: Illegal IO type!"
      ExtType (PtrET t          )
        | isFunExtType t          -> do
                                       align <- alignment CFunPtrPT
                                       return (bitSize CFunPtrPT, align)
        | otherwise               -> do
                                       align <- alignment CPtrPT
                                       return (bitSize CPtrPT, align)
      ExtType (DefinedET _ _    ) ->
        interr "GenBind.sizeAlignOf: Should never get a defined type"
{- OLD:
                                     do
                                       align <- alignment CPtrPT
                                       return (bitSize CPtrPT, align)
        -- FIXME: The defined type could be a function pointer!!!
 -}
      ExtType (PrimET pt        ) -> do
                                       align <- alignment pt
                                       return (bitSize pt, align)
      ExtType UnitET              -> voidFieldErr (posOf cdecl)
      SUType su                   ->
        do
          let (fields, tag) = structMembers su
          fields' <- let ide = structName su
                     in
                     if (not . null $ fields) || isNothing ide
                     then return fields
                     else do                              -- get the real...
                       tag' <- findTag (fromJust ide)      -- ...definition
                       case tag' of
                         Just (StructUnionCT su') -> return
                                                     (fst . structMembers $ su')
                         _                       -> return fields
          sizeAlignOfStructPad fields' tag
  where
    bitSize et | sz < 0    = BitSize 0  (-sz)   -- size is in bits
               | otherwise = BitSize sz 0
               where
                 sz = CInfo.size et

-- | apply the given alignment constraint at the given offset
--
-- * if the alignment constraint is negative or zero, it is the alignment
--   constraint for a bitfield
--
-- * the third argument gives the platform-specific bitfield alignment
--
alignOffset :: BitSize -> Int -> Int -> BitSize
alignOffset offset@(BitSize octetOffset bitOffset) align bitfieldAlignment
  | align > 0 && bitOffset /= 0 =               -- close bitfield first
    alignOffset (BitSize (octetOffset + (bitOffset + 7) `div` 8) 0) align
                bitfieldAlignment
  | align > 0 && bitOffset == 0 =               -- no bitfields involved
    BitSize (((octetOffset - 1) `div` align + 1) * align) 0
  | bitOffset == 0                              -- start a bitfield
    || overflowingBitfield      =               -- .. or overflowing bitfield
    alignOffset offset bitfieldAlignment bitfieldAlignment
  | otherwise                   =               -- stays in current bitfield
    offset
  where
    bitsPerBitfield     = CInfo.size CIntPT * 8
    overflowingBitfield = bitOffset - align > bitsPerBitfield
                                    -- note, `align' is negative


-- constant folding
-- ----------------

-- | evaluate a constant expression
--
-- FIXME: this is a bit too simplistic, as the range of expression allowed as
--        constant expression varies depending on the context in which the
--        constant expression occurs
--
evalConstCExpr :: CExpr -> GB ConstResult
evalConstCExpr (CComma _ at) =
  illegalConstExprErr (posOf at) "a comma expression"
evalConstCExpr (CAssign _ _ _ at) =
  illegalConstExprErr (posOf at) "an assignment"
evalConstCExpr (CCond b (Just t) e _) =
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
evalConstCExpr c@(CCast _ _ _) =
  evalCCast c
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
evalConstCExpr (CVar ide''' at) =
  do
    (cobj, _) <- findValueObj ide''' False
    case cobj of
      EnumCO ide'' (CEnum _ (Just enumrs) _ _) ->
        liftM IntResult $ enumTagValue ide'' enumrs 0
      _                             ->
        todo $ "GenBind.evalConstCExpr: variable names not implemented yet " ++
               show (posOf at)
  where
    -- FIXME: this is not very nice; instead, CTrav should have some support
    --        for determining enum tag values (but then, constant folding needs
    --        to be moved to CTrav, too)
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
                        IntResult val'' -> return val''
                        FloatResult _  ->
                          illegalConstExprErr (posOf exp) "a float result"
        if ide == ide'
          then                  -- found the right enumerator
            return val'
          else                  -- continue down the enumerator list
            enumTagValue ide enumrs (val' + 1)
evalConstCExpr (CConst c) = evalCConst c

evalCCast :: CExpr -> GB ConstResult
evalCCast (CCast decl expr _) = do
    compType <- extractCompType False False decl
    evalCCast' compType (getConstInt expr)
  where
    getConstInt (CConst (CIntConst (CInteger i _ _) _)) = i
    getConstInt _ = todo "GenBind.evalCCast: Casts are implemented only for integral constants"

evalCCast' :: CompType -> Integer -> GB ConstResult
evalCCast' (ExtType (PrimET primType)) i
  | isIntegralCPrimType primType = return $ IntResult i
evalCCast' _ _ = todo "GenBind.evalCCast': Only integral trivial casts are implemented"

evalCConst :: CConst -> GB ConstResult
evalCConst (CIntConst   i _ ) = return $ IntResult (getCInteger i)
evalCConst (CCharConst  c _ ) = return $ IntResult (getCCharAsInt c)
evalCConst (CFloatConst _ _ ) =
  todo "GenBind.evalCConst: Float conversion from literal misses."
evalCConst (CStrConst   _ at) =
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
applyBin _    CMulOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x * y)
applyBin _    CMulOp (FloatResult x)
                     (FloatResult y) = return $ FloatResult (x * y)
applyBin _    CDivOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x `div` y)
applyBin _    CDivOp (FloatResult x)
                     (FloatResult y) = return $ FloatResult (x / y)
applyBin _    CRmdOp (IntResult   x)
                     (IntResult   y) = return$ IntResult (x `mod` y)
applyBin cpos CRmdOp (FloatResult _)
                     (FloatResult _) =
  illegalConstExprErr cpos "a % operator applied to a float"
applyBin _    CAddOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x + y)
applyBin _    CAddOp (FloatResult x)
                     (FloatResult y) = return $ FloatResult (x + y)
applyBin _    CSubOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x - y)
applyBin _    CSubOp (FloatResult x)
                     (FloatResult y) = return $ FloatResult (x - y)
applyBin _    CShlOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x * 2^y)
applyBin cpos CShlOp (FloatResult _)
                     (FloatResult _) =
  illegalConstExprErr cpos "a << operator applied to a float"
applyBin _    CShrOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x `div` 2^y)
applyBin cpos CShrOp (FloatResult _)
                     (FloatResult _) =
  illegalConstExprErr cpos "a >> operator applied to a float"
applyBin _    COrOp  (IntResult   x)
                     (IntResult   y) = return $ IntResult (x .|. y)
applyBin _    CAndOp (IntResult   x)
                     (IntResult   y) = return $ IntResult (x .&. y)
applyBin _    _      (IntResult   _)
                     (IntResult   _) =
  todo "GenBind.applyBin: Not yet implemented operator in constant expression."
applyBin _    _      (FloatResult _)
                     (FloatResult _) =
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
applyUnary _    CPlusOp    arg             = return arg
applyUnary _    CMinOp     (IntResult   x) = return (IntResult (-x))
applyUnary _    CMinOp     (FloatResult x) = return (FloatResult (-x))
applyUnary _    CCompOp    _               =
  todo "GenBind.applyUnary: ~ not yet implemented."
applyUnary _    CNegOp     (IntResult   x) =
  let r = toInteger . fromEnum $ (x == 0)
  in return (IntResult r)
applyUnary cpos CNegOp     (FloatResult _) =
  illegalConstExprErr cpos "! applied to a float"


-- auxilliary functions
-- --------------------

-- | print trace message
--
traceGenBind :: String -> GB ()
traceGenBind  = putTraceStr traceGenBindSW

-- | generic lookup
--
lookupBy      :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq x  = fmap snd . find (eq x . fst)


-- error messages
-- --------------

unknownFieldErr          :: Position -> Ident -> GB a
unknownFieldErr cpos ide  =
  raiseErrorCTExc (posOf ide)
    ["Unknown member name!",
     "The structure has no member called `" ++ identToString ide
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
     "The type specifier of this declaration is not supported by your \
     \combination of C compiler and Haskell compiler."
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
     "Attempt to access member `" ++ identToString ide ++ "' in something not",
     "a structure or union."]

ptrExpectedErr     :: Position -> GB a
ptrExpectedErr pos  =
  raiseErrorCTExc pos
    ["Expected a pointer object!",
     "Attempt to dereference a non-pointer object or to use it in a `pointer' \
     \hook."]

funPtrExpectedErr     :: Position -> GB a
funPtrExpectedErr pos  =
  raiseErrorCTExc pos
    ["Expected a pointer-to-function object!",
     "Attempt to use a non-pointer object in a `call' or `fun' hook."]

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

offsetBitfieldErr :: Position -> GB a
offsetBitfieldErr pos =
    raiseErrorCTExc pos ["Illegal offset of a bit field!",
                         "Bit fields do not necessarily lie " ++
                         "on a whole-byte boundary."]

offsetDerefErr :: Position -> GB a
offsetDerefErr pos =
    raiseErrorCTExc pos ["Disallowed offset of using a dereference!",
                         "While calculable, it would almost certainly " ++
                         "be confusing to give the offset from the " ++
                         "beginning of a not-obviously-related struct"]

resMarshIllegalInErr     :: Position -> GB a
resMarshIllegalInErr pos  =
  raiseErrorCTExc pos
    ["Malformed result marshalling!",
     "An \"in\" marshaller is not allowed for the function result type.",
     "Note that \"out\" marshallers are specified *after* the type, like:",
     " {# fun ... -> `MyType' mkMyType #} "]

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

noDftMarshErr :: Position -> String -> String -> [ExtType] -> GB a
noDftMarshErr pos inOut hsTy cTys  =
  raiseErrorCTExc pos
    ["Missing " ++ inOut ++ " marshaller!",
     "There is no default marshaller for this combination of Haskell and \
     \C type:",
     "Haskell type: " ++ hsTy,
     "C type      : " ++ concat (intersperse " " (map showExtType cTys))]

undefEnumErr :: Position -> GB a
undefEnumErr pos = raiseErrorCTExc pos ["Incomplete enum type!"]
