{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
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
--    bool                      -> CBool
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
import qualified Prelude

-- standard libraries
import Data.Char     (toLower, isSpace)
import Data.Function (on)
import Data.IORef    (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import System.IO     (withFile, hPutStrLn, IOMode(..))
import System.Exit   (ExitCode(..))
import System.Directory (removeFile)
import System.Process (readProcessWithExitCode, system)
import Data.List     (deleteBy, groupBy, sortBy, intersperse, find, nubBy,
                      intercalate, isPrefixOf, isInfixOf, foldl')
import Data.Map      (lookup)
import Data.Maybe    (isNothing, isJust, fromJust, fromMaybe)
import Data.Bits     ((.|.), (.&.))
import Control.Arrow (second)
import Control.Monad (when, unless, liftM, mapAndUnzipM, zipWithM, forM)
import Data.Ord      (comparing)
import qualified Foreign.Storable as Storable (Storable(alignment),
                                               Storable(sizeOf))
import Foreign    (Ptr, FunPtr)
import Foreign.C

-- Language.C / compiler toolkit
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Pretty
import Text.PrettyPrint.HughesPJ (render)
import Data.Errors
import C2HS.Config (PlatformSpec(..))
import C2HS.State  (getSwitch)
import C2HS.Switches   (platformSB)


-- C->Haskell
import C2HS.State  (CST, errorsPresent, showErrors, fatal,
                   SwitchBoard(..), Traces(..), putTraceStr)
import C2HS.C

-- friends
import C2HS.CHS   (CHSModule(..), CHSFrag(..), CHSHook(..), CHSParm(..),
                   CHSMarsh, CHSArg(..), CHSAccess(..), CHSAPath(..),
                   CHSTypedefInfo, Direction(..),
                   CHSPtrType(..), showCHSParm, apathToIdent, apathRootIdent)
import C2HS.C.Info      (CPrimType(..))
import C2HS.Gen.Monad    (TransFun, transTabToTransFun, HsObject(..), GB,
                          GBState(..), Wrapper(..),
                   initialGBState, setContext, getPrefix, getReplacementPrefix,
                   delayCode, getDelayedCode, ptrMapsTo, queryPtr, objIs,
                   sizeIs, querySize, queryClass, queryPointer,
                   mergeMaps, dumpMaps, queryEnum, isEnum,
                   queryTypedef, isC2HSTypedef,
                   queryDefaultMarsh, isDefaultMarsh, addWrapper, getWrappers,
                   addHsDependency, getHsDependencies)


-- Module import alias.
imp :: String
imp = "C2HSImp"

impm :: String -> String
impm s = imp ++ "." ++ s


-- default marshallers
-- -------------------

-- FIXME:
-- - we might have a dynamically extended table in the monad if needed (we
--   could marshall enums this way and also save the 'id' marshallers for
--   pointers defined via (newtype) pointer hooks)
-- - the checks for the Haskell types are quite kludgy

stringIn :: String
stringIn = "\\s f -> " ++ impm "withCStringLen" ++ " s " ++
           "(\\(p, n) -> f (p, fromIntegral n))"

-- | determine the default "in" marshaller for the given Haskell and C types
--
lookupDftMarshIn :: String -> [ExtType] -> GB CHSMarsh
lookupDftMarshIn "Bool"   [PrimET pt] | isIntegralCPrimType pt = do
  addHsDependency "Foreign.Marshal.Utils"
  return $ Just (Left cFromBoolIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isIntegralHsType hsTy
                                      &&isIntegralCPrimType pt =
  return $ Just (Left cIntConvIde, CHSValArg)
lookupDftMarshIn hsTy     [PrimET pt] | isFloatHsType hsTy
                                      &&isFloatCPrimType pt    =
  return $ Just (Left cFloatConvIde, CHSValArg)
lookupDftMarshIn "Char" [PrimET CCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCharToCCharIde, CHSValArg)
lookupDftMarshIn "Char" [PrimET CUCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCharToCUCharIde, CHSValArg)
lookupDftMarshIn "Char" [PrimET CSCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCharToCSCharIde, CHSValArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT)] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left withCStringIde, CHSIOArg)
lookupDftMarshIn "CString" [PtrET (PrimET CCharPT)]             =
  return $ Just (Right "flip ($)", CHSIOArg)
lookupDftMarshIn "String" [PtrET (PrimET CCharPT), PrimET pt]
  | isIntegralCPrimType pt = do
  addHsDependency "Foreign.C.String"
  return $ Just (Right stringIn , CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]
  | isIntegralHsType hsTy && isIntegralCPrimType pt = do
  addHsDependency "Foreign.Marshal.Utils"
  return $ Just (Right $ impm "with" ++ " . fromIntegral", CHSIOArg)
lookupDftMarshIn hsTy     [PtrET (PrimET pt)]
  | isFloatHsType hsTy && isFloatCPrimType pt = do
  addHsDependency "Foreign.Marshal.Utils"
  return $ Just (Right $ impm "with" ++ " . realToFrac", CHSIOArg)
lookupDftMarshIn "Bool"   [PtrET (PrimET pt)]
  | isIntegralCPrimType pt = do
  addHsDependency "Foreign.Marshal.Utils"
  return $ Just (Right $ impm "with" ++ " . fromBool", CHSIOArg)
lookupDftMarshIn hsTy [PtrET UnitET] | "Ptr " `isPrefixOf` hsTy =
  return $ Just (Left idIde, CHSValArg)
lookupDftMarshIn hsTy [PrimET (CAliasedPT tds hsAlias _)] = do
  mm <- queryDefaultMarsh $ (In, tds, False)
  case mm of
    Nothing -> if hsTy == hsAlias
               then return $ Just (Left idIde, CHSValArg)
               else return Nothing
    Just m -> return $ Just m
lookupDftMarshIn hsTy [PtrET (PrimET (CAliasedPT tds hsAlias _pt))] = do
  mm <- queryDefaultMarsh $ (In, tds, True)
  case mm of
    Nothing -> if hsTy == hsAlias
               then return $ Just (Left idIde, CHSValArg)
               else return Nothing
    Just m -> return $ Just m
-- Default case deals with:
lookupDftMarshIn hsty _ = do
  om <- readCT objmap
  isenum <- queryEnum hsty
  case (isenum, (internalIdent hsty) `lookup` om) of
    --  1. enumeration hooks
    (True, Nothing) ->
      return $ Just (Right "fromIntegral . fromEnum", CHSValArg)
    --  2. naked and newtype pointer hooks
    (False, Just (Pointer CHSPtr _)) ->
      return $ Just (Left idIde, CHSValArg)
    --  3. foreign pointer hooks
    (False, Just (Pointer (CHSForeignPtr _) False)) -> do
      addHsDependency "Foreign.ForeignPtr"
      return $ Just (Left withForeignPtrIde, CHSIOArg)
    --  4. foreign newtype pointer hooks
    (False, Just (Pointer (CHSForeignPtr _) True)) ->
      return $ Just (Right $ "with" ++ hsty, CHSIOArg)
    _ -> return Nothing
-- FIXME: handle array-list conversion


-- | determine the default "out" marshaller for the given Haskell and C types
--
lookupDftMarshOut :: String -> [ExtType] -> GB CHSMarsh
lookupDftMarshOut "()"     _                                    =
  return $ Just (Left voidIde, CHSVoidArg)
lookupDftMarshOut "Bool"   [PrimET pt] | isIntegralCPrimType pt = do
  addHsDependency "Foreign.Marshal.Utils"
  return $ Just (Left cToBoolIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isIntegralHsType hsTy
                                      && isIntegralCPrimType pt =
  return $ Just (Left cIntConvIde, CHSValArg)
lookupDftMarshOut hsTy     [PrimET pt] | isFloatHsType hsTy
                                      && isFloatCPrimType pt    =
  return $ Just (Left cFloatConvIde, CHSValArg)
lookupDftMarshOut "Char" [PrimET CCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCCharToCharIde, CHSValArg)
lookupDftMarshOut "Char" [PrimET CUCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCUCharToCharIde, CHSValArg)
lookupDftMarshOut "Char" [PrimET CSCharPT] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left castCSCharToCharIde, CHSValArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT)] = do
  addHsDependency "Foreign.C.String"
  return $ Just (Left peekCStringIde, CHSIOArg)
lookupDftMarshOut "CString" [PtrET (PrimET CCharPT)] =
  return $ Just (Left returnIde, CHSIOArg)
lookupDftMarshOut "String" [PtrET (PrimET CCharPT), PrimET pt]
  | isIntegralCPrimType pt = do
  addHsDependency "Foreign.C.String"
  return $ Just (Right $ "\\(s, n) -> " ++ impm "peekCStringLen" ++
                         " (s, fromIntegral n)",
                 CHSIOArg)
lookupDftMarshOut hsTy [PtrET UnitET] | "Ptr " `isPrefixOf` hsTy =
  return $ Just (Left idIde, CHSValArg)
lookupDftMarshOut hsTy [PrimET (CAliasedPT tds hsAlias _)] = do
  mm <- queryDefaultMarsh $ (Out, tds, False)
  case mm of
    Nothing -> if hsTy == hsAlias
               then return $ Just (Left idIde, CHSValArg)
               else return Nothing
    Just m -> return $ Just m
lookupDftMarshOut hsTy [PtrET (PrimET (CAliasedPT tds hsAlias _pt))] = do
  mm <- queryDefaultMarsh $ (Out, tds, True)
  case mm of
    Nothing -> if hsTy == hsAlias
               then return $ Just (Left idIde, CHSValArg)
               else return Nothing
    Just m -> return $ Just m
lookupDftMarshOut hsty _ = do
  om <- readCT objmap
  isenum <- queryEnum hsty
  res <- case (isenum, (internalIdent hsty) `lookup` om) of
    --  1. enumeration hooks
    (True, Nothing) -> return $ Just (Right "toEnum . fromIntegral", CHSValArg)
    --  2. naked and newtype pointer hooks
    (False, Just (Pointer CHSPtr _)) -> return $ Just (Left idIde, CHSValArg)
    --  3. foreign pointer hooks
    (False, Just (Pointer (CHSForeignPtr Nothing) False)) -> do
      addHsDependency "Foreign.ForeignPtr"
      return $ Just (Left newForeignPtr_Ide, CHSIOArg)
    (False, Just (Pointer (CHSForeignPtr (Just fin)) False)) -> do
      code <- newForeignPtrCode fin
      return $ Just (Right $ code, CHSIOArg)
    --  4. foreign newtype pointer hooks
    (False, Just (Pointer (CHSForeignPtr Nothing) True)) -> do
      addHsDependency "Foreign.ForeignPtr"
      return $ Just (Right $ "\\x -> " ++ impm "newForeignPtr_ x >>= " ++
                             " (return . " ++ hsty ++ ")",
                     CHSIOArg)
    (False, Just (Pointer (CHSForeignPtr (Just fin)) True)) -> do
      code <- newForeignPtrCode fin
      return $ Just (Right $ "\\x -> " ++ code ++ " x >>= (return . " ++
                     hsty ++ ")", CHSIOArg)
    _ -> return Nothing
  return res
-- FIXME: add combination, such as "peek" plus "cIntConv" etc
-- FIXME: handle array-list conversion

newForeignPtrCode :: (Ident, Maybe Ident) -> GB String
newForeignPtrCode (cide, ohside) = do
  (_, cide') <- findFunObj cide True
  let fin = (identToString cide') `maybe` identToString $ ohside
  addHsDependency "Foreign.ForeignPtr"
  return $ impm "newForeignPtr" ++ " " ++ fin


-- | check for integral Haskell types
--
isIntegralHsType :: String -> Bool
isIntegralHsType "Int"     = True
isIntegralHsType "Int8"    = True
isIntegralHsType "Int16"   = True
isIntegralHsType "Int32"   = True
isIntegralHsType "Int64"   = True
isIntegralHsType "Word8"   = True
isIntegralHsType "Word16"  = True
isIntegralHsType "Word32"  = True
isIntegralHsType "Word64"  = True
isIntegralHsType "CShort"  = True
isIntegralHsType "CUShort" = True
isIntegralHsType "CInt"    = True
isIntegralHsType "CUInt"   = True
isIntegralHsType "CLong"   = True
isIntegralHsType "CULong"  = True
isIntegralHsType _         = False

-- | check for floating Haskell types
--
isFloatHsType :: String -> Bool
isFloatHsType "Float"   = True
isFloatHsType "Double"  = True
isFloatHsType "CFloat"  = True
isFloatHsType "CDouble" = True
isFloatHsType _         = False

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
                                CULongPT, CULLongPT, CBoolPT])

-- | check for floating C types
--
isFloatCPrimType :: CPrimType -> Bool
isFloatCPrimType  = (`elem` [CFloatPT, CDoublePT, CLDoublePT])

-- | standard conversions
--
voidIde, cFromBoolIde, cToBoolIde, cIntConvIde, cFloatConvIde,
  withCStringIde, peekCStringIde, idIde,
  newForeignPtr_Ide, withForeignPtrIde, returnIde,
  castCharToCCharIde, castCharToCUCharIde, castCharToCSCharIde,
  castCCharToCharIde, castCUCharToCharIde, castCSCharToCharIde :: Ident
voidIde             = internalIdent $ impm "void"       -- never appears in the output
cFromBoolIde        = internalIdent $ impm "fromBool"
cToBoolIde          = internalIdent $ impm "toBool"
cIntConvIde         = internalIdent "fromIntegral"
cFloatConvIde       = internalIdent "realToFrac"
withCStringIde      = internalIdent $ impm "withCString"
peekCStringIde      = internalIdent $ impm "peekCString"
idIde               = internalIdent "id"
newForeignPtr_Ide   = internalIdent $ impm "newForeignPtr_"
withForeignPtrIde   = internalIdent $ impm "withForeignPtr"
returnIde           = internalIdent "return"
castCharToCCharIde  = internalIdent $ impm "castCharToCChar"
castCharToCUCharIde = internalIdent $ impm "castCharToCUChar"
castCharToCSCharIde = internalIdent $ impm "castCharToCSChar"
castCCharToCharIde  = internalIdent $ impm "castCCharToChar"
castCUCharToCharIde = internalIdent $ impm "castCUCharToChar"
castCSCharToCharIde = internalIdent $ impm "castCSCharToChar"


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
expandHooks :: AttrC -> CHSModule ->
               CST s (CHSModule, String, [Wrapper], String)
expandHooks ac mod' = do
  (_, res) <- runCT (expandModule mod') ac initialGBState
  return res

expandModule :: CHSModule -> GB (CHSModule, String, [Wrapper], String)
expandModule (CHSModule mfrags)  =
  do
    -- expand hooks
    --
    traceInfoExpand
    frags'       <- expandFrags mfrags
    hsdeps       <- getHsDependencies
    let frags'' = addImports frags' hsdeps
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
        wraps <- getWrappers
        return (CHSModule (frags'' ++ delayedFrags), chi, wraps, warnmsgs)
  where
    traceInfoExpand = putTraceStr tracePhasesSW
                        ("...expanding binding hooks...\n")
    traceInfoErr    = putTraceStr tracePhasesSW
                        ("...error(s) detected.\n")
    traceInfoOK     = putTraceStr tracePhasesSW
                        ("...successfully completed.\n")

-- | add import declarations for modules required internally by C2HS
--
addImports :: [CHSFrag] -> [String] -> [CHSFrag]
addImports fs imps = before ++ impfrags ++ after
  where impfrags = sp ++ concatMap impfrag imps ++ sp
        sp = [CHSVerb "\n" imppos]
        impfrag i =
          [CHSVerb ("import qualified " ++ i ++ " as " ++ imp) imppos,
           CHSVerb "\n" imppos]
        (before, after) = doSplit 0 Nothing False [] fs
        imppos = posOf $ last before

        -- Find the appropriate location to put the import
        -- declarations.  This relies heavily on the details of the
        -- CHS parser to deal with Haskell comments, but a simple
        -- approach like this seems to be a better idea than using
        -- haskell-src-exts or something like that, mostly because
        -- none of the Haskell parsing packages deal with *all* GHC
        -- extensions.  The approach taken here isn't pretty, but it
        -- seems to work.
        doSplit :: Int -> Maybe Int -> Bool ->
                   [CHSFrag] -> [CHSFrag] -> ([CHSFrag], [CHSFrag])
        doSplit _ Nothing   _ _ [] = (fs, [])
        doSplit _ (Just ln) _ _ [] = splitAt (ln-1) fs
        doSplit 0 mln wh acc (f@(CHSVerb s pos) : fs')
          | "--" `isPrefixOf` s = doSplit 0 mln wh (f:acc) fs'
          | s == "{-"           = doSplit 1 mln wh (f:acc) fs'
          | wh && "where" `isInfixOf` s = (reverse (f:acc), fs')
          | "module" `isPrefixOf` (dropWhile isSpace s) =
              if (" where" `isInfixOf` s || ")where" `isInfixOf` s)
              then (reverse (f:acc), fs')
              else doSplit 0 mln True (f:acc) fs'
          | otherwise = if null (dropWhile isSpace s) || isJust mln
                        then doSplit 0 mln wh (f:acc) fs'
                        else doSplit 0 (Just $ posRow pos) wh (f:acc) fs'
        doSplit cdep mln wh acc (f@(CHSVerb s _) : fs')
          | s == "-}" = doSplit (cdep-1) mln wh (f:acc) fs'
          | s == "{-" = doSplit (cdep+1) mln wh (f:acc) fs'
          | otherwise = doSplit cdep     mln wh (f:acc) fs'
        doSplit cdep mln wh acc (f:fs') = doSplit cdep mln wh (f:acc) fs'


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
    addExtTypeDependency ty
    return $ "(" ++ showExtType ty ++ ")"
  where
    traceInfoType         = traceGenBind "** Type hook:\n"
    traceInfoDump decl ty = traceGenBind $
      "Declaration\n" ++ show decl ++ "\ntranslates to\n"
      ++ showExtType ty ++ "\n"
expandHook (CHSAlignof ide _) _ =
  do
    traceInfoAlignof
    decl <- findAndChaseDeclOrTag ide False True  -- no indirection, but shadows
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
    decl <- findAndChaseDeclOrTag ide False True  -- no indirection, but shadows
    (sz, _) <- sizeAlignOf decl
    traceInfoDump (render $ pretty decl) sz
    return $ show (padBits sz)
  where
    traceInfoSizeof         = traceGenBind "** Sizeof hook:\n"
    traceInfoDump decl sz = traceGenBind $
      "Size of declaration\n" ++ show decl ++ "\nis "
      ++ show (padBits sz) ++ "\n"
expandHook (CHSEnumDefine _ _ _ _) _ =
  interr $ "Binding generation error : enum define hooks " ++
           "should be eliminated via preprocessing "
expandHook (CHSEnum cide oalias chsTrans emit oprefix orepprefix derive pos) _ =
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
    enumDef enum hide trans emit (map identToString derive) pos
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
    callImport hook isPure isUns [] ideLexeme hsLexeme cdecl' Nothing pos
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
    set_get <- setGet pos CHSGet offsets Nothing ptrTy Nothing

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
      "** Indirect call hook for `" ++
      identToString (apathToIdent apath) ++ "':\n"
    traceValueType et  = traceGenBind $
      "Type of accessed value: " ++ showExtType et ++ "\n"
expandHook (CHSFun isPure isUns _ inVarTypes (CHSRoot _ ide)
            oalias ctxt parms parm pos) hkpos =
  do
    traceEnter
    traceGenBind $ "ide = '" ++ show ide ++ "'\n"
    traceGenBind $ "inVarTypes = " ++ show inVarTypes ++ "\n"
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
        isWrapped (CHSParm _ _ twovals _ w _ _)
          | twovals = [w, w]
          | otherwise = [w]
        isWrapped _ = [False]
        wrapped   = Just $ concatMap isWrapped parms

    varTypes <- convertVarTypes hsLexeme pos inVarTypes
    callImport callHook isPure isUns varTypes (identToString cide)
      fiLexeme cdecl' wrapped pos

    extTy <- extractFunType pos cdecl' True wrapped
    funDef isPure hsLexeme fiLexeme extTy varTypes
      ctxt parms parm Nothing pos hkpos
  where
    traceEnter = traceGenBind $
      "** Fun hook for `" ++ identToString ide ++ "':\n"
expandHook (CHSFun isPure isUns _ _ apath oalias ctxt parms parm pos) hkpos =
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

    set_get <- setGet pos CHSGet offsets Nothing ptrTy Nothing
    funDef isPure hsLexeme fiLexeme (FunET ptrTy $ purify ty) []
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
    onewtype <- apathNewtypeName path
    traceGenBind $ "onewtype = " ++ show onewtype ++ "\n"
    (decl, offsets) <- accessPath path
    traceDepth offsets
    ty <- extractSimpleType False pos decl
    traceValueType ty
    setGet pos access offsets (isArrDecl decl) ty onewtype
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
        extractCompType True True False decl >>= \ compTy ->
        case compTy of
          (VarFunET  _) -> variadicErr pos pos
          (IOET      _) ->
            interr "GenBind.expandHook(CHSOffsetOf): Illegal type!"
          (UnitET     ) -> voidFieldErr pos
          (DefinedET _ _) -> return offset
          (PrimET (CUFieldPT _)) -> offsetBitfieldErr pos
          (PrimET (CSFieldPT _)) -> offsetBitfieldErr pos
          _             -> return offset
    checkType _ _ = offsetDerefErr pos
expandHook hook@(CHSPointer isStar cName oalias ptrKind isNewtype oRefType emit
                 pos) _ =
  do
    traceInfoPointer
    let hsIde  = fromMaybe cName oalias
        hsName = identToString hsIde

    hsIde `objIs` Pointer ptrKind isNewtype     -- register Haskell object
    decl <- findAndChaseDeclOrTag cName False True
    (sz, _) <- sizeAlignOfPtr decl
    hsIde `sizeIs` (padBits sz)
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
            []     -> do
                             cDecl <- chaseDecl cNameFull (not isStar)
                             et    <- extractPtrType cDecl
                             traceInfoPtrType et
                             let et' = adjustPtr isStar et
                             when (isVariadic et')
                                  (variadicErr pos (posOf cDecl))
                             return (showExtType et', isFunExtType et')
            hsType -> return (identsToString hsType, False)
            -- FIXME: it is not possible to determine whether `hsType'
            --   is a function; we would need to extend the syntax to
            --   allow `... -> fun HSTYPE' to explicitly mark function
            --   types if this ever becomes important
        traceInfoHsType hsName hsType
        doFinalizer hook ptrKind (if isNewtype then hsName else "()")
        pointerDef isStar cNameFull hsName ptrKind isNewtype hsType isFun emit
      Right tag -> do                           -- found a tag definition
        let cNameFull = tagName tag
        traceInfoCName "tag definition" cNameFull
        unless isStar $                         -- tags need an explicit `*'
          ptrExpectedErr (posOf cName)
        let hsType = case oRefType of
                       []      -> "()"
                       hsType' -> identsToString hsType'
        traceInfoHsType hsName hsType
        doFinalizer hook ptrKind (if isNewtype then hsName else "()")
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
    identsToString :: [Ident] -> String
    identsToString = intercalate " " . map identToString

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
expandHook (CHSConst cIde _) _ =
  do
    traceGenBind "** Constant hook:\n"
    Just (ObjCO cdecl) <- findObj cIde
    let (Just ini) = initDeclr cdecl
    return . show . pretty $ ini
expandHook (CHSTypedef cIde hsIde pos) _ =
  do
    traceGenBind $ "** Typedef hook: " ++ identToString cIde ++
      " -> " ++ identToString hsIde ++ "\n"
    let def = "__c2hs_typedef__" ++
              identToString cIde ++ "__" ++ identToString hsIde
    Just (ObjCO cdecl) <- findObj $ internalIdent def
    st <- extractCompType True True False cdecl
    et <- case st of
      PrimET e -> return e
      _ -> typeDefaultErr pos
    cIde `isC2HSTypedef` (hsIde, et)
    return ""
expandHook (CHSDefault dir hsTy cTy cPtr marsh pos) _ =
  do
    traceGenBind $ "** Default hook: " ++ hsTy ++ " [" ++ cTy ++
      (if cPtr then " *" else "") ++ "]\n"
    mtypedef <- queryTypedef $ internalIdent cTy
    case mtypedef of
      Nothing -> typeDefaultErr pos
      Just (tdide, _) -> do
        let def = "__c2hs_typedef__" ++ cTy ++ "__" ++ identToString tdide
        Just (ObjCO cdecl) <- findObj $ internalIdent def
        st <- extractCompType True True False cdecl
        case st of
          PrimET _ -> do
            (dir, cTy, cPtr) `isDefaultMarsh` marsh
            return ""
          _ -> typeDefaultErr pos

apathNewtypeName :: CHSAPath -> GB (Maybe Ident)
apathNewtypeName path = do
    let ide = apathRootIdent path
    pm <- readCT ptrmap
    case (True, ide) `lookup` pm of
      Nothing -> return Nothing
      Just (hsty, _) -> do
        om <- readCT objmap
        let hside = internalIdent hsty
        case hside `lookup` om of
          Just (Pointer _ True) -> return (Just hside)
          _ -> return Nothing

-- | produce code for an enumeration
--
-- * an extra instance declaration is required when any of the enumeration
--   constants is explicitly assigned a value in its definition
--
-- * the translation function strips prefixes where possible (different
--   enumerators maye have different prefixes)
--
enumDef :: CEnum -> String -> TransFun -> Bool -> [String] -> Position
        -> GB String
enumDef (CEnum _ Nothing _ _) _ _ _ _ pos = undefEnumErr pos
enumDef (CEnum _ (Just list) _ _) hident trans emit userDerive _ =
  do
    (list', enumAuto) <- evalTagVals list
    let enumVals = map (\(Just i, e) -> (i, e)) $ filter (isJust . fst) $
                   fixTags [(trans ide, cexpr) | (ide, cexpr) <- list']
        defHead  = enumHead hident
        defBody  = enumBody (length defHead - 2) enumVals
        dataDef = if emit then defHead ++ defBody else ""
        inst     = makeDerives
                   (if enumAuto then "Enum" : userDerive else userDerive) ++
                   "\n" ++
                   if enumAuto
                   then ""
                   else enumInst hident enumVals
    isEnum hident
    return $ dataDef ++ inst
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
  (-) a b = cInteger (getCInteger a - getCInteger b)
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
  [ "instance Enum " ++ wrap ident ++ " where"
  , succDef
  , predDef
  , enumFromToDef
  , enumFromDef
  , fromDef
  , toDef
  ]
  where
    wrap s = if ' ' `elem` s then "(" ++ s ++ ")" else s
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
callImport :: CHSHook -> Bool -> Bool -> [ExtType] -> String ->
              String -> CDecl -> Maybe [Bool] -> Position -> GB ()
callImport hook isPure isUns varTypes ideLexeme hsLexeme cdecl owrapped pos =
  do
    -- compute the external type from the declaration, and delay the foreign
    -- export declaration
    --
    extType <- extractFunType pos cdecl isPure owrapped
    header  <- getSwitch headerSB
    let bools@(boolres, boolargs) = boolArgs extType
        needwrapper1 = boolres || or boolargs
        (needwrapper2, wraps) = case owrapped of
          Nothing -> (False, replicate (numArgs extType) False)
          Just ws -> if or ws
                     then (True, ws)
                     else (False, replicate (numArgs extType) False)
        ide = if needwrapper1 || needwrapper2
              then "__c2hs_wrapped__" ++ ideLexeme
              else ideLexeme
    addExtTypeDependency extType
    delayCode hook (foreignImport (extractCallingConvention cdecl)
                    header ide hsLexeme isUns extType varTypes)
    when (needwrapper1 || needwrapper2) $
      addWrapper ide ideLexeme cdecl wraps bools pos
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
    addExtTypeDependency ty
    delayCode hook (foreignImportDyn (extractCallingConvention cdecl)
                    ideLexeme hsLexeme isUns ty)
    traceFunType ty
  where
    traceFunType et = traceGenBind $
      "Imported function type: " ++ showExtType et ++ "\n"

-- | Haskell code for the foreign import declaration needed by a call hook
--
foreignImport :: CallingConvention -> String -> String -> String -> Bool ->
                 ExtType -> [ExtType] -> String
foreignImport cconv header ident hsIdent isUnsafe ty vas =
  "foreign import " ++ showCallingConvention cconv ++ " " ++ safety
  ++ " " ++ show entity ++
  "\n  " ++ hsIdent ++ " :: " ++ showExtFunType ty vas ++ "\n"
  where
    safety = if isUnsafe then "unsafe" else "safe"
    entity | null header = ident
           | otherwise   = header ++ " " ++ ident

-- | Haskell code for the foreign import dynamic declaration needed by
-- a call hook
--
foreignImportDyn :: CallingConvention -> String -> String -> Bool ->
                    ExtType -> String
foreignImportDyn cconv _ident hsIdent isUnsafe ty  =
  "foreign import " ++ showCallingConvention cconv ++ " " ++ safety
    ++ " \"dynamic\"\n  " ++
    hsIdent ++ " :: " ++ impm "FunPtr" ++ "( " ++
    showExtType ty ++ " ) -> " ++ showExtType ty ++ "\n"
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
       -> [ExtType]          -- simplified declaration of the C function
       -> Maybe String       -- type context of the new Haskell function
       -> [CHSParm]          -- parameter marshalling description
       -> CHSParm            -- result marshalling description
       -> Maybe String       -- optional additional marshaller for first arg
       -> Position           -- source location of the hook
       -> Position           -- source location of the start of the hook
       -> GB String          -- Haskell code in text form
funDef isPure hsLexeme fiLexeme extTy varExtTys octxt parms
       parm@(CHSParm _ hsParmTy _ _ _ _ _) marsh2 pos hkpos =
  do
    when (countPlus parms > 1 || isPlus parm) $ illegalPlusErr pos
    (parms', parm', isImpure) <- addDftMarshaller pos parms parm extTy varExtTys

    traceMarsh parms' parm' isImpure
    marshs <- zipWithM marshArg [1..] parms'
    let
      sig       = hsLexeme ++ " :: " ++ funTy parms' parm' ++ "\n"
      funArgs   = [funArg   | (funArg, _, _, _, _)   <- marshs, funArg   /= ""]
      marshIns  = [marshIn  | (_, marshIn, _, _, _)  <- marshs]
      callArgs  = [callArg  | (_, _, cs, _, _)  <- marshs, callArg <- cs]
      marshOuts = [marshOut | (_, _, _, marshOut, _) <- marshs, marshOut /= ""]
      retArgs   = [retArg   | (_, _, _, _, retArg)   <- marshs, retArg   /= ""]
      funHead   = hsLexeme ++ join funArgs ++ " =\n" ++
                  if isPure && isImpure
                  then "  " ++ impm "unsafePerformIO" ++ " $\n"
                  else ""
      call      = if isPure
                  then "  let {res = " ++ fiLexeme ++ joinCallArgs ++
                       "} in" ++ (if isPure then " res `seq`" else "") ++ "\n"
                  else "  " ++ fiLexeme ++ joinCallArgs ++ case parm of
                    CHSParm _ "()" _ Nothing _ _ _ -> " >>\n"
                    _                        ->
                      if countPlus parms == 1
                      then " >>\n" else " >>= \\res ->\n"
      joinCallArgs = case marsh2 of
                        Nothing -> join callArgs
                        Just _  -> join ("b1'" : drop 1 callArgs)
      mkMarsh2  = case marsh2 of
                        Nothing -> ""
                        Just m  -> "  " ++ m ++ " " ++
                                   join (take 1 callArgs) ++
                                   " >>= \\b1' ->\n"
      marshRes  = if countPlus parms == 1
                  then ""
                  else case parm' of
                    CHSParm _ _ _twoCVal (Just (_, CHSVoidArg)) _ _ _ -> ""
                    CHSParm _ _ _twoCVal (Just (omBody, CHSIOVoidArg)) _ _ _ ->
                      "  " ++ marshBody omBody ++ " res >> \n"
                    CHSParm _ _ _twoCVal (Just (omBody, CHSIOArg)) _ _ _ ->
                      "  " ++ marshBody omBody ++ " res >>= \\res' ->\n"
                    CHSParm _ _ _twoCVal (Just (omBody, CHSValArg)) _ _ _ ->
                      "  let {res' = " ++ marshBody omBody ++ " res} in" ++
                      (if isPure then " res `seq`" else "") ++ "\n"
                    CHSParm _ _ _ Nothing _ _ _ ->
                      interr "GenBind.funDef: marshRes: no default?"

      marshBody (Left ide) = identToString ide
      marshBody (Right str) = "(" ++ str ++ ")"

      retArgs'  = case parm' of
                    CHSParm _ _ _ (Just (_, CHSVoidArg))   _ _ _ -> retArgs
                    CHSParm _ _ _ (Just (_, CHSIOVoidArg)) _ _ _ -> retArgs
                    _                                        ->
                      if countPlus parms == 0 then "res'":retArgs else retArgs
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

    when (isPure && isImpure) $ addHsDependency "System.IO.Unsafe"
    return $ pad $ sig ++ funHead ++ funBody
  where
    countPlus :: [CHSParm] -> Int
    countPlus = sum . map (\p -> if isPlus p then 1 else 0)
    isPlus CHSPlusParm = True
    isPlus _           = False
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
        showComment str = if null str
                          then ""
                          else " --" ++ str ++ "\n"
        ctxt   = case octxt of
                   Nothing      -> ""
                   Just ctxtStr -> ctxtStr ++ " => "
        argTys = ["(" ++ ty ++ ")" ++ showComment c |
                     CHSParm im ty _ _  _ _ c <- parms', notVoid im]
        resTys = ["(" ++ ty ++ ")" |
                     CHSParm _  ty _ om _ _ _ <- parm':parms', notVoid om]
        resTup = let
                   comment = case parm' of
                       CHSParm _ _ _ _ _ _ c -> c
                   (lp, rp) = if isPure && length resTys == 1
                              then ("", "")
                              else ("(", ")")
                   io       = if isPure then "" else "IO "
                 in
                 io ++ lp ++ concat (intersperse ", " resTys) ++ rp ++
                 showComment comment

      in
      ctxt ++ concat (intersperse " -> " (argTys ++ [resTup]))
      where
        notVoid Nothing
          = interr "GenBind.funDef: No default marshaller?"
        notVoid (Just (_, kind)) = kind /= CHSVoidArg && kind /= CHSIOVoidArg
    --
    -- for an argument marshaller, generate all "in" and "out" marshalling
    -- code fragments
    --
    marshArg i (CHSParm (Just (imBody, imArgKind)) _ twoCVal
                        (Just (omBody, omArgKind)) _ _ _    ) = do
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
        retArg   = if omArgKind == CHSVoidArg || omArgKind == CHSIOVoidArg
                   then "" else outBndr

        marshBody (Left ide) = identToString ide
        marshBody (Right str) = "(" ++ str ++ ")"
      return (funArg, marshIn, callArgs, marshOut, retArg)
    marshArg i CHSPlusParm = do
      msize <- querySize $ internalIdent hsParmTy
      case msize of
        Nothing -> interr "Missing size for \"+\" parameter allocation!"
        Just sz -> do
          let a = "a" ++ show (i :: Int)
              bdr1 = a ++ "'"
              bdr2 = a ++ "''"
              marshIn = impm "mallocForeignPtrBytes" ++ " " ++ show sz ++
                        " >>= \\" ++ bdr2 ++
                        " -> " ++ impm "withForeignPtr" ++ " " ++ bdr2 ++
                        " $ \\" ++ bdr1 ++ " -> "
          addHsDependency "Foreign.ForeignPtr"
          return ("", marshIn, [bdr1], "", hsParmTy ++ " " ++ bdr2)
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
addDftMarshaller :: Position -> [CHSParm] -> CHSParm -> ExtType -> [ExtType]
                 -> GB ([CHSParm], CHSParm, Bool)
addDftMarshaller pos parms parm extTy varExTys = do
  let (resTy, argTys)  = splitFunTy extTy varExTys
  (parm' , isImpure1) <- checkResMarsh parm resTy
  (parms', isImpure2) <- addDft parms argTys
  return (parms', parm', isImpure1 || isImpure2)
  where
    -- the result marshalling may not use an "in" marshaller and can only have
    -- one C value
    --
    -- * a default marshaller maybe used for "out" marshalling
    --
    checkResMarsh (CHSParm (Just _) _  _    _       _ pos' _) _   =
      resMarshIllegalInErr      pos'
    checkResMarsh (CHSParm _        _  True _       _ pos' _) _   =
      resMarshIllegalTwoCValErr pos'
    checkResMarsh (CHSParm _        ty _    omMarsh _ pos' c) cTy = do
      (imMarsh', _       ) <- addDftVoid Nothing
      (omMarsh', isImpure) <- addDftOut pos' omMarsh ty [cTy]
      return (CHSParm imMarsh' ty False omMarsh' False pos' c, isImpure)
    --
    splitFunTy (FunET UnitET ty) vts = splitFunTy ty vts
    splitFunTy (FunET ty1 ty2) vts = let (resTy, argTys) = splitFunTy ty2 vts
                                   in (resTy, ty1:argTys)
    splitFunTy (VarFunET ty2) vts = let (resTy, argTys) = splitFunTy ty2 []
                                    in (resTy, argTys ++ vts)
    splitFunTy resTy _ = (resTy, [])
    --
    -- match Haskell with C arguments (and results)
    --
    addDft ((CHSPlusParm):parms'') (_:cTys) = do
      (parms', _) <- addDft parms'' cTys
      return (CHSPlusParm : parms', True)
    addDft ((CHSParm imMarsh hsTy False omMarsh _ p c):parms'') (cTy:cTys) = do
      (imMarsh', isImpureIn ) <- addDftIn p imMarsh hsTy [cTy]
      (omMarsh', isImpureOut) <- addDftVoid omMarsh
      (parms'  , isImpure   ) <- addDft parms'' cTys
      return (CHSParm imMarsh' hsTy False omMarsh' False p c : parms',
              isImpure || isImpureIn || isImpureOut)
    addDft ((CHSParm imMarsh hsTy True  omMarsh _ p c):parms'') (ct1:ct2:cts) =
      do
      (imMarsh', isImpureIn ) <- addDftIn p imMarsh hsTy [ct1, ct2]
      (omMarsh', isImpureOut) <- addDftVoid omMarsh
      (parms'  , isImpure   ) <- addDft parms'' cts
      return (CHSParm imMarsh' hsTy True omMarsh' False p c : parms',
              isImpure || isImpureIn || isImpureOut)
    addDft [] [] = return ([], False)
    addDft ((CHSParm _ _ _ _ _ pos' _):_) [] =
      marshArgMismatchErr pos' "This parameter is in excess of the C arguments."
    addDft [] (_:_) =
      marshArgMismatchErr pos "Parameter marshallers are missing."
    --
    addDftIn _ imMarsh@(Just (_, kind)) _ _ = return (imMarsh, kind == CHSIOArg)
    addDftIn pos' _imMarsh@Nothing hsTy cts = do
      marsh <- lookupDftMarshIn hsTy cts
      when (isNothing marsh) $
        noDftMarshErr pos' "\"in\"" hsTy cts
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    addDftOut _ omMarsh@(Just (_, kind)) _ _ =
      return (omMarsh, kind == CHSIOArg)
    addDftOut pos' _omMarsh@Nothing hsTy cts = do
      marsh <- lookupDftMarshOut hsTy cts
      when (isNothing marsh) $
        noDftMarshErr pos' "\"out\"" hsTy cts
      return (marsh, case marsh of {Just (_, kind) -> kind == CHSIOArg})
    --
    -- add void marshaller if no explict one is given
    --
    addDftVoid marsh@(Just (_, kind)) = return (marsh, kind == CHSIOArg)
    addDftVoid Nothing = do
      return (Just (Left voidIde, CHSVoidArg), False)

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
    derefDeclr (CDeclr oid (CPtrDeclr _ _: derived') asm ats n) =
      return $ CDeclr oid derived' asm ats n
    derefDeclr (CDeclr _oid _unexp_deriv _ _ n) = ptrExpectedErr (posOf n)

-- | replaces a decleration by its alias if any
--
-- * the alias inherits any field size specification that the original
--   declaration may have
--
-- * declaration must have exactly one declarator
--
replaceByAlias                                :: CDecl -> GB CDecl
replaceByAlias cdecl@(CDecl _ [(_, _, sz)] _at)  =
  do
    ocdecl <- checkForAlias cdecl
    case ocdecl of
      Nothing                                  -> return cdecl
      Just (CDecl specs [(declr, init', _)] at) ->   -- form of an alias
        return $ CDecl specs [(declr, init', sz)] at

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
cdecl@(CDecl _ []             _) `declNamed` _   =
  errorAtPos (posOf cdecl) ["GenBind.declNamed: Abstract declarator in structure!"]
cdecl                            `declNamed` _   =
  errorAtPos (posOf cdecl) ["GenBind.declNamed: More than one declarator!"]

-- | Haskell code for writing to or reading from a struct
--
setGet :: Position -> CHSAccess -> [BitSize] -> Maybe Int ->
          ExtType -> Maybe Ident -> GB String
setGet pos access offsets arrSize ty onewtype =
  do
    let pre = case (access, onewtype) of
          (CHSSet, Nothing) -> "(\\ptr val -> do {"
          (CHSGet, Nothing) -> "(\\ptr -> do {"
          (CHSSet, Just ide) ->
            "(\\(" ++ identToString ide ++ " ptr) val -> do {"
          (CHSGet, Just ide) ->
            "(\\(" ++ identToString ide ++ " ptr) -> do {"
    body <- setGetBody (reverse offsets)
    return $ pre ++ body ++ "})"
  where
    setGetBody [BitSize offset bitOffset] =
      do
        bf <- checkType ty
        case bf of
          Nothing      -> case access of       -- not a bitfield
                            CHSGet -> peekOp offset ty arrSize
                            CHSSet -> pokeOp offset ty "val" arrSize
--FIXME: must take `bitfieldDirection' into account
          Just (_, bs) -> case access of       -- a bitfield
                            CHSGet -> do
                              op <- peekOp offset ty arrSize
                              addHsDependency "Data.Bits"
                              addHsDependency "Foreign.C.Types"
                              return $ "val <- " ++ op ++ extractBitfield
                            CHSSet -> do
                              op <- peekOp offset ty arrSize
                              op2 <- pokeOp offset ty "val'" arrSize
                              addHsDependency "Data.Bits"
                              addHsDependency "Foreign.C.Types"
                              return $ "org <- " ++ op ++ insertBitfield
                                      ++ op2
            where
              -- we have to be careful here to ensure proper sign extension;
              -- in particular, shifting right followed by anding a mask is
              -- *not* sufficient; instead, we exploit in the following that
              -- `shiftR' performs sign extension
              --
              extractBitfield = "; return $ (val `" ++ impm "shiftL" ++ "` ("
                                ++ bitsPerField ++ " - "
                                ++ show (bs + bitOffset) ++ ")) `"
                                ++ impm "shiftR" ++ "` ("
                                ++ bitsPerField ++ " - " ++ show bs
                                ++ ")"
              bitsPerField    = show $ size CIntPT * 8
              --
              insertBitfield  = "; let {val' = (org " ++ impm ".&." ++ " "
                                ++ middleMask ++ ") " ++ impm ".|."
                                ++ " (val `" ++ impm "shiftL" ++ "` "
                                ++ show bitOffset ++ ")}; "
              middleMask      = "fromIntegral (((maxBound::" ++ impm "CUInt"
                                ++ ") `" ++ impm "shiftL" ++ "` "
                                ++ show bs ++ ") `" ++ impm "rotateL" ++ "` "
                                ++ show bitOffset ++ ")"
    setGetBody (BitSize offset 0 : offsetsrem) =
      do
        code <- setGetBody offsetsrem
        addHsDependency "Foreign.Storable"
        return $ "ptr <- " ++ impm "peekByteOff" ++ " ptr "
                 ++ show offset ++ "; " ++ code
    setGetBody (BitSize _      _ : _      ) =
      derefBitfieldErr pos
    --
    -- check that the type can be marshalled and compute extra operations for
    -- bitfields
    --
    checkType (VarFunET  _    )          = variadicErr pos pos
    checkType (IOET      _    )          = errorAtPos pos ["GenBind.setGet: Illegal \
                                                            \type!"]
    checkType (UnitET         )          = voidFieldErr pos
    checkType (DefinedET _ _  )          = return Nothing-- can't check further
    checkType (PrimET    (CUFieldPT bs)) = return $ Just (False, bs)
    checkType (PrimET    (CSFieldPT bs)) = return $ Just (True , bs)
    checkType _                          = return Nothing
    --
    peekOp off (PrimET CBoolPT) Nothing = do
      addHsDependency "Foreign.Marshal.Utils"
      addHsDependency "Foreign.C.Types"
      addHsDependency "Foreign.Storable"
      return $ impm "toBool" ++ " `fmap` (" ++ impm "peekByteOff"
               ++ " ptr " ++ show off ++ " :: IO " ++ impm "CInt" ++ ")"
    peekOp off t Nothing = do
      addHsDependency "Foreign.Storable"
      addExtTypeDependency t
      return $ impm "peekByteOff" ++ " ptr " ++ show off
               ++ " :: IO " ++ showExtType t
    peekOp off t (Just _) = do
      addHsDependency "Foreign.Ptr"
      addExtTypeDependency t
      return $ "return $ ptr `" ++ impm "plusPtr" ++ "` " ++ show off ++
               " :: IO " ++ showExtType t
    pokeOp off (PrimET CBoolPT) var Nothing = do
      addHsDependency "Foreign.Marshal.Utils"
      addHsDependency "Foreign.C.Types"
      addHsDependency "Foreign.Storable"
      return $ impm "pokeByteOff" ++ " ptr " ++ show off
               ++ " (" ++ impm "fromBool" ++ " " ++
               var ++ " :: " ++ impm "CInt" ++ ")"
    pokeOp off t var Nothing = do
      addHsDependency "Foreign.Storable"
      addExtTypeDependency t
      return $ impm "pokeByteOff" ++ " ptr " ++ show off ++ " (" ++ var ++ " :: " ++
                                                  showExtType t ++ ")"
    pokeOp off t var (Just sz) = do
      addHsDependency "Foreign.Ptr"
      addHsDependency "Foreign.Marshal.Array"
      addExtTypeDependency t
      return $ impm "copyArray" ++ " (ptr `" ++ impm "plusPtr" ++ "` "
               ++ show off ++ ") (" ++
               var ++ " :: " ++ showExtType t ++ ") " ++ show sz

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
  -- ====> NEED TO DO DEPENDENCIES HERE!
  do
    let ptrArg  = if isNewtype
                  then hsName           -- abstract type
                  else hsType           -- concrete type
        ptrCon  = case ptrKind of
                    CHSPtr | isFun -> impm "FunPtr"
                    _              -> impm $ show ptrKind
        ptrType = ptrCon ++ " (" ++ ptrArg ++ ")"
        thePtr  = (isStar, cNameFull)
    case ptrKind of
      CHSPtr          -> addHsDependency "Foreign.Ptr"
      CHSForeignPtr _ -> do
        addHsDependency "Foreign.ForeignPtr"
        addHsDependency "Foreign.Ptr"
      CHSStablePtr    -> addHsDependency "Foreign.StablePtr"
    case ptrKind of
      CHSForeignPtr _ -> do
        thePtr `ptrMapsTo` (impm "Ptr (" ++ ptrArg ++ ")",
                            impm "Ptr (" ++ ptrArg ++ ")")
      _               -> thePtr `ptrMapsTo` (hsName, hsName)
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
        | isForeign ptrKind =
          "\nwith" ++ hsName ++ " :: " ++
          hsName ++ " -> (" ++ impm "Ptr" ++ " " ++ hsName
          ++ " -> IO b) -> IO b" ++
          "\n" ++ "with" ++ hsName ++ " (" ++ hsName ++
          " fptr) = " ++ impm "withForeignPtr" ++ " fptr"
        | otherwise                = ""
      isForeign (CHSForeignPtr _) = True
      isForeign _                 = False

-- | generate a foreign pointer finalizer import declaration that is
-- put into the delayed code
--
doFinalizer :: CHSHook -> CHSPtrType -> String -> GB ()
doFinalizer hook (CHSForeignPtr (Just (cide, ohside))) ptrHsIde = do
  (ObjCO cdecl, cide') <- findFunObj cide True
  let finCIde  = identToString cide'
      finHsIde = finCIde `maybe` identToString $ ohside
      cdecl'   = cide' `simplifyDecl` cdecl
  header <- getSwitch headerSB
  addHsDependency "Foreign.ForeignPtr"
  delayCode hook (finalizerImport (extractCallingConvention cdecl')
                  header finCIde finHsIde ptrHsIde)
  traceFunType ptrHsIde
  where
    traceFunType et = traceGenBind $
      "Imported finalizer function type: " ++ et ++ "\n"
doFinalizer _ _ _ = return ()

-- | Haskell code for the foreign import declaration needed by foreign
-- pointer finalizers.
--
finalizerImport :: CallingConvention -> String -> String -> String ->
                   String -> String
finalizerImport cconv header ident hsIdent hsPtrName  =
  "foreign import " ++ showCallingConvention cconv ++ " " ++ show entity ++
  "\n  " ++ hsIdent ++ " :: " ++ impm "FinalizerPtr" ++ " " ++ hsPtrName ++ "\n"
  where
    entity | null header = "&" ++ ident
           | otherwise   = header ++ " &" ++ ident

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
      toMethodName = case typeName of
        ""   -> errorAtPos pos ["GenBind.classDef: Illegal identifier!"]
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
              ""   -> errorAtPos pos ["GenBind.classDef: Illegal identifier - 2!"]
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
             | SUET      CStructUnion           -- structure or union
             deriving Show

instance Eq ExtType where
  (FunET     t1 t2) == (FunET     t1' t2') = t1 == t1' && t2 == t2'
  (IOET      t    ) == (IOET      t'     ) = t == t'
  (PtrET     t    ) == (PtrET     t'     ) = t == t'
  (DefinedET _  s ) == (DefinedET _   s' ) = s == s'
  (PrimET    t    ) == (PrimET    t'     ) = t == t'
  (VarFunET  t    ) == (VarFunET  t'     ) = t == t'
  (SUET (CStruct _ i _ _ _)) == (SUET (CStruct _ i' _ _ _)) = i == i'
  UnitET            == UnitET              = True

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

boolArgs :: ExtType -> (Bool, [Bool])
boolArgs (FunET a rest@(FunET _ _)) =
  let (res, as) = boolArgs rest in (res, boolArg a : as)
boolArgs (FunET a (IOET res)      ) = boolArgs (FunET a res)
boolArgs (FunET a (PrimET CBoolPT)) = (True, [boolArg a])
boolArgs (FunET a _               ) = (False, [boolArg a])
boolArgs _                          = (False, [])

boolArg :: ExtType -> Bool
boolArg (PrimET CBoolPT) = True
boolArg _                = False


-- | pretty print an external type
--
-- * a previous version of this function attempted to not print unnecessary
--   brackets; this however doesn't work consistently due to `DefinedET'; so,
--   we give up on the idea (preferring simplicity)
--
showExtType :: ExtType -> String
showExtType (FunET UnitET res)      = showExtType res
showExtType (FunET arg res)         = "(" ++ showExtType arg ++ " -> "
                                      ++ showExtType res ++ ")"
showExtType (VarFunET res)          = "( ... -> " ++ showExtType res ++ ")"
showExtType (IOET t)                = "(IO " ++ showExtType t ++ ")"
showExtType (PtrET t)               = let ptrCon = if isFunExtType t
                                                   then impm "FunPtr"
                                                   else impm "Ptr"
                                      in
                                      "(" ++ ptrCon ++ " " ++ showExtType t
                                      ++ ")"
showExtType (DefinedET _ str)       = "(" ++ str ++ ")"
showExtType (PrimET CPtrPT)         = "(" ++ impm "Ptr" ++ " ())"
showExtType (PrimET CFunPtrPT)      = "(" ++ impm "FunPtr" ++ " ())"
showExtType (PrimET CCharPT)        = impm "CChar"
showExtType (PrimET CUCharPT)       = impm "CUChar"
showExtType (PrimET CSCharPT)       = impm "CSChar"
showExtType (PrimET CIntPT)         = impm "CInt"
showExtType (PrimET CShortPT)       = impm "CShort"
showExtType (PrimET CLongPT)        = impm "CLong"
showExtType (PrimET CLLongPT)       = impm "CLLong"
showExtType (PrimET CUIntPT)        = impm "CUInt"
showExtType (PrimET CUShortPT)      = impm "CUShort"
showExtType (PrimET CULongPT)       = impm "CULong"
showExtType (PrimET CULLongPT)      = impm "CULLong"
showExtType (PrimET CFloatPT)       = impm "CFloat"
showExtType (PrimET CDoublePT)      = impm "CDouble"
showExtType (PrimET CLDoublePT)     = impm "CLDouble"
showExtType (PrimET CBoolPT)        = impm "CInt{-bool-}"
showExtType (PrimET (CSFieldPT bs)) = impm "CInt{-:" ++ show bs ++ "-}"
showExtType (PrimET (CUFieldPT bs)) = impm "CUInt{-:" ++ show bs ++ "-}"
showExtType (PrimET (CAliasedPT _ hs _)) = hs
showExtType UnitET                  = "()"
showExtType (SUET _)                = "(" ++ impm "Ptr" ++ " ())"

addExtTypeDependency :: ExtType -> GB ()
addExtTypeDependency (FunET UnitET res) = addExtTypeDependency res
addExtTypeDependency (FunET arg res) = do
  addExtTypeDependency arg
  addExtTypeDependency res
addExtTypeDependency (VarFunET res) = addExtTypeDependency res
addExtTypeDependency (IOET t) = addExtTypeDependency t
addExtTypeDependency (PtrET t) = do
  addHsDependency "Foreign.Ptr"
  addExtTypeDependency t
addExtTypeDependency (PrimET CPtrPT) =    addHsDependency "Foreign.Ptr"
addExtTypeDependency (PrimET CFunPtrPT) = addHsDependency "Foreign.Ptr"
addExtTypeDependency (PrimET (CAliasedPT _ _ _)) = return ()
addExtTypeDependency (PrimET _) =         addHsDependency "Foreign.C.Types"
addExtTypeDependency (SUET _) =           addHsDependency "Foreign.Ptr"
addExtTypeDependency _ = return ()

showExtFunType :: ExtType -> [ExtType] -> String
showExtFunType (FunET UnitET res) _ = showExtType res
showExtFunType (FunET arg res) vas =
  "(" ++ showExtType arg ++ " -> " ++ showExtFunType res vas ++ ")"
showExtFunType (VarFunET res) [] = showExtFunType res []
showExtFunType t@(VarFunET _) (va:vas) =
  "(" ++ showExtType va ++ " -> " ++ showExtFunType t vas ++ ")"
showExtFunType (IOET t) vas = "(IO " ++ showExtFunType t vas ++ ")"
showExtFunType t _ = showExtType t

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
extractFunType :: Position -> CDecl -> Bool -> Maybe [Bool] -> GB ExtType
extractFunType pos cdecl isPure wrapped =
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
    let wrap = case wrapped of
          Just w  -> w
          Nothing -> repeat False
    argTypes <- zipWithM (extractCompType False True) wrap args
    return $ foldr FunET resultType argTypes


-- | compute a non-struct/union type from the given declaration
--
-- * the declaration may have at most one declarator
--
extractSimpleType :: Bool -> Position -> CDecl -> GB ExtType
extractSimpleType isResult _ cdecl  = extractCompType isResult True False cdecl

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
  ct <- extractCompType False False False cdecl
  case ct of
    SUET _ -> return UnitET
    _      -> return ct

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
extractCompType :: Bool -> Bool -> Bool -> CDecl -> GB ExtType
extractCompType isResult usePtrAliases isPtr cdecl@(CDecl specs' declrs ats) =
  if length declrs > 1
  then errorAtPos (posOf cdecl) ["GenBind.extractCompType: Too many declarators!"]
  else case declrs of
    [(Just declr, _, sz)] | isPtr || isPtrDeclr declr -> ptrType declr
                          | isFunDeclr declr -> funType
                          | otherwise        -> aliasOrSpecType sz
    _                                        -> aliasOrSpecType Nothing
  where
    -- handle explicit pointer types
    --
    ptrType declr = do
      tracePtrType
      let declrs' = if isPtr    -- remove indirection
                    then declr
                    else dropPtrDeclr declr
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
          ct <- extractCompType False usePtrAliases False cdecl'
          return $ case ct of
            SUET _  -> PtrET UnitET
            _ -> PtrET ct
    --
    -- handle explicit function types
    --
    -- FIXME: we currently regard any functions as being impure (ie, being IO
    --        functions); is this ever going to be a problem?
    --
    funType = do
      traceFunType
      -- ??? IS Nothing OK HERE?
      extractFunType (posOf cdecl) cdecl False Nothing

    makeAliasedCompType :: Ident -> CHSTypedefInfo -> GB ExtType
    makeAliasedCompType cIde (hsIde, et) = do
      return $ PrimET $
        CAliasedPT (identToString cIde) (identToString hsIde) et

    --
    -- handle all types, which are not obviously pointers or functions
    --
    aliasOrSpecType :: Maybe CExpr -> GB ExtType
    aliasOrSpecType sz = do
      traceAliasOrSpecType sz
      case checkForOneAliasName cdecl of
        Nothing   -> specType (posOf cdecl) specs' sz
        Just ide  -> do                    -- this is a typedef alias
          oDefault <- queryTypedef ide
          case oDefault of
            Just tdefault -> makeAliasedCompType ide tdefault
            Nothing -> do
              traceAlias ide
              oHsRepr <- queryPtr (False, ide) -- check for pointer hook alias
              case oHsRepr of
                Just repr | usePtrAliases
                   -> ptrAlias repr    -- found a pointer hook alias
                _  -> do               -- skip current alias (only one)
                        cdecl' <- getDeclOf ide
                        let CDecl specs [(declr, init', _)] at =
                              ide `simplifyDecl` cdecl'
                            sdecl = CDecl specs [(declr, init', sz)] at
                            -- propagate `sz' down (slightly kludgy)
                        extractCompType isResult usePtrAliases False sdecl
    --
    -- compute the result for a pointer alias
    --
    ptrAlias (repr1, repr2) =
      return $ DefinedET cdecl (if isResult then repr2 else repr1)
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
            ([bool]                      , PrimET CBoolPT   ),
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
             bool     = CBoolType   undefined
             signed   = CSignedType undefined
             unsigned = CUnsigType  undefined
             enum     = CEnumType   undefined undefined

convertVarTypes :: String -> Position -> [String] -> GB [ExtType]
convertVarTypes base pos ts = do
  let vaIdent i = internalIdent $ "__c2hs__vararg__" ++ base ++ "_" ++ show i
      ides = map vaIdent [0..length ts - 1]
      doone ide = do
        Just (ObjCO cdecl) <- findObj ide
        return cdecl
  cdecls <- mapM doone ides
  forM cdecls $ \cdecl -> do
    st <- extractCompType True True False cdecl
    case st of
      SUET _ -> variadicTypeErr pos
      _ -> return st

-- | compute the complex (external) type determined by a list of type specifiers
--
-- * may not be called for a specifier that defines a typedef alias
--
specType :: Position -> [CDeclSpec] -> Maybe CExpr -> GB ExtType
specType cpos specs'' osize =
  let tspecs = [ts | CTypeSpec ts <- specs'']
  in case lookupTSpec tspecs typeMap of
    Just et | isUnsupportedType et -> unsupportedTypeSpecErr cpos
            | isNothing osize      -> return et   -- not a bitfield
            | otherwise            -> bitfieldSpec tspecs et osize  -- bitfield
    Nothing                        ->
      case tspecs of
        [CSUType   cu _] -> return $ SUET cu                 -- struct or union
        [CEnumType _  _] -> return $ PrimET CIntPT           -- enum
        [CTypeDef  _  _] -> errorAtPos cpos ["GenBind.specType: Illegal typedef alias!"]
        _                -> illegalTypeSpecErr cpos
  where
    lookupTSpec = lookupBy matches
    --
    -- can't be a bitfield (yet)
    isUnsupportedType (PrimET et) = et /= CBoolPT && size et == 0
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
    eqSpec (CBoolType   _) (CBoolType   _) = True
    eqSpec (CSignedType _) (CSignedType _) = True
    eqSpec (CUnsigType  _) (CUnsigType  _) = True
    eqSpec (CSUType   _ _) (CSUType   _ _) = True
    eqSpec (CEnumType _ _) (CEnumType _ _) = True
    eqSpec (CTypeDef  _ _) (CTypeDef  _ _) = True
    eqSpec _               _               = False
    --
    bitfieldSpec :: [CTypeSpec] -> ExtType -> Maybe CExpr -> GB ExtType
    bitfieldSpec tspecs et (Just sizeExpr) =  -- never called with 'Nothing'
      do
        PlatformSpec {bitfieldIntSignedPS = bitfieldIntSigned} <- getPlatform
        let pos = posOf sizeExpr
        sizeResult <- evalConstCExpr sizeExpr
        case sizeResult of
          FloatResult _     -> illegalConstExprErr pos "a float result"
          IntResult   size' -> do
            let sz = fromInteger size'
            case et of
              PrimET CUIntPT                      -> returnCT $ CUFieldPT sz
              PrimET CIntPT
                |  [signed]      `matches` tspecs
                || [signed, int] `matches` tspecs -> returnCT $ CSFieldPT sz
                |  [int]         `matches` tspecs ->
                  returnCT $ if bitfieldIntSigned then CSFieldPT sz
                                                  else CUFieldPT sz
              _                                   -> illegalFieldSizeErr pos
            where
              returnCT = return . PrimET
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
    funEndAttrs [(Just ((CDeclr _ (CFunDeclr _ _ _ : _) _ attrs _)), _, _)] =
      attrs
    funEndAttrs _ = []

    -- attrs appearing within the declarator of a function pointer. As an
    -- example:
    -- typedef int (__stdcall *fp)();
    funPtrAttrs [(Just ((CDeclr _ (CPtrDeclr _ _ :
                                   CFunDeclr _ attrs _ : _) _ _ _)), _, _)] =
      attrs
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
addBitSize :: BitSize -> BitSize -> BitSize
addBitSize (BitSize o1 b1) (BitSize o2 b2) =
  BitSize (o1 + o2 + overflow * size CIntPT) rest
  where
    bitsPerBitfield  = size CIntPT * 8
    (overflow, rest) = (b1 + b2) `divMod` bitsPerBitfield

-- | multiply a bit size by a constant (gives size of an array)
--
-- * not sure if this makes sense if the number of bits is non-zero.
--
scaleBitSize                  :: Int -> BitSize -> BitSize
scaleBitSize n (BitSize o1 b1) = BitSize (n * o1 + overflow) rest
  where
    bitsPerBitfield  = size CIntPT * 8
    (overflow, rest) = (n * b1) `divMod` bitsPerBitfield

-- | pad any storage unit that is partially used by a bitfield
--
padBits               :: BitSize -> Int
padBits (BitSize o 0)  = o
padBits (BitSize o _)  = o + size CIntPT

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
    (sz, align)        <- sizeAlignOf       (last decls)
    let sizeOfStruct  = alignOffset offset align bitfieldAlignment
                        `addBitSize` sz
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
    (sz, align) <- sizeAlignOfStruct decls tag
    let b = size CIntPT
    return (alignOffset sz b b, align)

-- | compute the size and alignment constraint of a given C declaration
--
sizeAlignOf       :: CDecl -> GB (BitSize, Int)
sizeAlignOfPtr    :: CDecl -> GB (BitSize, Int)
sizeAlignOfBase   :: Bool -> CDecl -> GB (BitSize, Int)
sizeAlignOfSingle :: Bool -> CDecl -> GB (BitSize, Int)
--
-- * we make use of the assertion that 'extractCompType' can only return a
--   'DefinedET' when the declaration is a pointer declaration
-- * for arrays, alignment is the same as for the base type and the size
--   is the size of the base type multiplied by the number of elements.
--   FIXME: I'm not sure whether anything of this is guaranteed by ISO C
--   and I have no idea what happens when an array-of-bitfield is
--   declared.  At this time I don't care.  -- U.S. 05/2006
--
sizeAlignOf = sizeAlignOfBase False
sizeAlignOfPtr = sizeAlignOfBase True
sizeAlignOfBase _ (CDecl dclspec
                         [(Just (CDeclr oide (CArrDeclr _ (CArrSize _ lexpr) _ :
                                              derived') _asm _ats n), init', expr)]
                         attr) =
  do
    (bitsize, align) <-
      sizeAlignOf (CDecl dclspec
                   [(Just (CDeclr oide derived' Nothing [] n), init', expr)]
                   attr)
    IntResult len <- evalConstCExpr lexpr
    return (fromIntegral len `scaleBitSize` bitsize, align)
sizeAlignOfBase _ cdecl@(CDecl _ [(Just (CDeclr _ (CArrDeclr _ (CNoArrSize _) _ :
                                             _) _ _ _), _init, _expr)] _) =
    errorAtPos (posOf cdecl) ["GenBind.sizeAlignOf: array of undeclared size."]
sizeAlignOfBase ptr cdecl = do
  traceAliasCheck
  case checkForOneAliasName cdecl of
    Nothing   -> sizeAlignOfSingle ptr cdecl
    Just ide  -> do                    -- this is a typedef alias
      traceAlias ide
      cdecl' <- getDeclOf ide
      let CDecl specs [(declr, init', _)] at = ide `simplifyDecl` cdecl'
          sdecl = CDecl specs [(declr, init', Nothing)] at
      sizeAlignOf sdecl
  where
    traceAliasCheck  = traceGenBind $ "extractCompType: checking for alias\n"
    traceAlias ide = traceGenBind $
      "extractCompType: found an alias called `" ++ identToString ide ++ "'\n"


sizeAlignOfSingle ptr cdecl = do
  ct <- extractCompType False False False cdecl
  case ct of
    FunET _ _ -> do
      align <- alignment CFunPtrPT
      return (bitSize CFunPtrPT, align)
    VarFunET _ -> do
      align <- alignment CFunPtrPT
      return (bitSize CFunPtrPT, align)
    IOET  _ -> errorAtPos (posOf cdecl) ["GenBind.sizeof: Illegal IO type!"]
    PtrET t
      | isFunExtType t -> do
        align <- alignment CFunPtrPT
        return (bitSize CFunPtrPT, align)
      | otherwise -> do
        align <- alignment CPtrPT
        return (bitSize CPtrPT, align)
    DefinedET _ _ ->
      errorAtPos (posOf cdecl) ["GenBind.sizeAlignOf: Should never get a defined type"]
    PrimET pt -> do
      align <- alignment pt
      return (bitSize pt, align)
    UnitET -> if ptr
              then do
                align <- alignment CPtrPT
                return (bitSize CPtrPT, align)
              else voidFieldErr (posOf cdecl)
    SUET su -> do
      let (fields, tag) = structMembers su
      fields' <- let ide = structName su
                 in if (not . null $ fields) || isNothing ide
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
                 sz = size et

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
    bitsPerBitfield     = size CIntPT * 8
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
    (sz, _) <- sizeAlignOf decl
    return $ IntResult (fromIntegral . padBits $ sz)
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
evalConstCExpr cdecl@(CVar ide''' at) =
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
      errorAtPos (posOf cdecl) ["GenBind.enumTagValue: enumerator not in declaration"]
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
    compType <- extractCompType False False False decl
    evalCCast' compType (getConstInt expr)
  where
    getConstInt (CConst (CIntConst (CInteger i _ _) _)) = i
    getConstInt _ = todo $ "GenBind.evalCCast: Casts are implemented " ++
                           "only for integral constants"

evalCCast' :: ExtType -> Integer -> GB ConstResult
evalCCast' (PrimET primType) i
  | isIntegralCPrimType primType = return $ IntResult i
evalCCast' _ _ = todo $ "GenBind.evalCCast': Only integral trivial " ++
                        "casts are implemented"

evalCConst :: CConst -> GB ConstResult
evalCConst (CIntConst   i _ ) = return $ IntResult (getCInteger i)
evalCConst (CCharConst  c@(C2HS.C.CChar _ _) _ ) =
  return $ IntResult (getCCharAsInt c)
evalCConst (CCharConst  (CChars cs _) _ ) = return $ IntResult (foldl' add 0 cs)
  where add tot ch = tot * 0x100 + fromIntegral (fromEnum ch)
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
applyBin pos  _      (IntResult   _)
                     (IntResult   _) =
  todo $ "GenBind.applyBin: Not yet implemented operator in constant expression. " ++ show pos
applyBin pos  _      (FloatResult _)
                     (FloatResult _) =
  todo $ "GenBind.applyBin: Not yet implemented operator in constant expression. " ++ show pos
applyBin pos    _      _ _             =
  errorAtPos pos ["GenBind.applyBinOp: Illegal combination!"]

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
applyUnary pos  CCompOp    _               =
  todo $ "GenBind.applyUnary: ~ not yet implemented. " ++ show pos
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

illegalTypeSpecErr      :: Position -> GB a
illegalTypeSpecErr cpos  =
  raiseErrorCTExc cpos
    ["Illegal type!",
     "The type specifiers of this declaration do not form a " ++
     "legal ANSI C(89) type."
    ]

unsupportedTypeSpecErr      :: Position -> GB a
unsupportedTypeSpecErr cpos  =
  raiseErrorCTExc cpos
    ["Unsupported type!",
     "The type specifier of this declaration is not supported by your " ++
     "combination of C compiler and Haskell compiler."
    ]

variadicErr          :: Position -> Position -> GB a
variadicErr pos cpos  =
  raiseErrorCTExc pos
    ["Variadic function!",
     "Calling variadic functions is not supported by the FFI; the function",
     "is defined at " ++ show cpos ++ "."]

variadicTypeErr          :: Position -> GB a
variadicTypeErr pos  =
  raiseErrorCTExc pos
    ["Variadic function argument type!",
     "Calling variadic functions is only supported for simple C types"]

typeDefaultErr      :: Position -> GB a
typeDefaultErr pos  =
  raiseErrorCTExc pos
    ["Internal type default error!",
     "Something went wrong."]

illegalPlusErr       :: Position -> GB a
illegalPlusErr pos  =
  raiseErrorCTExc pos
    ["Illegal plus parameter!",
     "The special parameter `+' may only be used in a single input " ++
     "parameter position in a function hook"]

illegalConstExprErr           :: Position -> String -> GB a
illegalConstExprErr cpos hint  =
  raiseErrorCTExc cpos ["Illegal constant expression!",
                        "Encountered " ++ hint ++ " in a constant expression,",
                        "which ANSI C89 does not permit."]

voidFieldErr      :: Position -> GB a
voidFieldErr cpos =
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
     "Attempt to dereference a non-pointer object or to use it in a " ++
     "`pointer' hook."]

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
     "There is no default marshaller for this combination of Haskell and " ++
     "C type:",
     "Haskell type: " ++ hsTy,
     "C type      : " ++ concat (intersperse " " (map showExtType cTys))]

undefEnumErr :: Position -> GB a
undefEnumErr pos = raiseErrorCTExc pos ["Incomplete enum type!"]


-- | size of primitive type of C
--
-- * negative size implies that it is a bit, not an octet size
--
size                :: CPrimType -> Int
size CPtrPT          = Storable.sizeOf (undefined :: Ptr ())
size CFunPtrPT       = Storable.sizeOf (undefined :: FunPtr ())
size CCharPT         = 1
size CUCharPT        = 1
size CSCharPT        = 1
size CIntPT          = Storable.sizeOf (undefined :: CInt)
size CShortPT        = Storable.sizeOf (undefined :: CShort)
size CLongPT         = Storable.sizeOf (undefined :: CLong)
size CLLongPT        = Storable.sizeOf (undefined :: CLLong)
size CUIntPT         = Storable.sizeOf (undefined :: CUInt)
size CUShortPT       = Storable.sizeOf (undefined :: CUShort)
size CULongPT        = Storable.sizeOf (undefined :: CULong)
size CULLongPT       = Storable.sizeOf (undefined :: CLLong)
size CFloatPT        = Storable.sizeOf (undefined :: Foreign.C.CFloat)
size CDoublePT       = Storable.sizeOf (undefined :: CDouble)
#if MIN_VERSION_base(4,2,0)
size CLDoublePT      = 0  --marks it as an unsupported type, see 'specType'
#else
size CLDoublePT      = Storable.sizeOf (undefined :: CLDouble)
#endif
size CBoolPT         = cBoolSize
size (CSFieldPT bs)  = -bs
size (CUFieldPT bs)  = -bs
size (CAliasedPT _ _ pt) = size pt


-- | alignment of C's primitive types
--
-- * more precisely, the padding put before the type's member starts when the
--   preceding component is a char
--
alignment                :: CPrimType -> GB Int
alignment CPtrPT          = return $ Storable.alignment (undefined :: Ptr ())
alignment CFunPtrPT       = return $ Storable.alignment (undefined :: FunPtr ())
alignment CCharPT         = return $ 1
alignment CUCharPT        = return $ 1
alignment CSCharPT        = return $ 1
alignment CIntPT          = return $ Storable.alignment (undefined :: CInt)
alignment CShortPT        = return $ Storable.alignment (undefined :: CShort)
alignment CLongPT         = return $ Storable.alignment (undefined :: CLong)
alignment CLLongPT        = return $ Storable.alignment (undefined :: CLLong)
alignment CUIntPT         = return $ Storable.alignment (undefined :: CUInt)
alignment CUShortPT       = return $ Storable.alignment (undefined :: CUShort)
alignment CULongPT        = return $ Storable.alignment (undefined :: CULong)
alignment CULLongPT       = return $ Storable.alignment (undefined :: CULLong)
alignment CFloatPT =
  return $ Storable.alignment (undefined :: Foreign.C.CFloat)
alignment CDoublePT       = return $ Storable.alignment (undefined :: CDouble)
#if MIN_VERSION_base(4,2,0)
alignment CLDoublePT      = interr "Info.alignment: CLDouble not supported"
#else
alignment CLDoublePT      = return $ Storable.alignment (undefined :: CLDouble)
#endif
alignment CBoolPT         = return cBoolSize
alignment (CSFieldPT bs)  = fieldAlignment bs
alignment (CUFieldPT bs)  = fieldAlignment bs
alignment (CAliasedPT _ _ pt) = alignment pt

-- | alignment constraint for a C bitfield
--
-- * gets the bitfield size (in bits) as an argument
--
-- * alignments constraints smaller or equal to zero are reserved for bitfield
--   alignments
--
-- * bitfields of size 0 always trigger padding; thus, they get the maximal
--   size
--
-- * if bitfields whose size exceeds the space that is still available in a
--   partially filled storage unit trigger padding, the size of a storage unit
--   is provided as the alignment constraint; otherwise, it is 0 (meaning it
--   definitely starts at the current position)
--
-- * here, alignment constraint /= 0 are somewhat subtle; they mean that is
--   the given number of bits doesn't fit in what's left in the current
--   storage unit, alignment to the start of the next storage unit has to be
--   triggered
--
fieldAlignment :: Int -> GB Int
fieldAlignment 0  = return $ - (size CIntPT - 1)
fieldAlignment bs =
  do
    PlatformSpec {bitfieldPaddingPS = bitfieldPadding} <- getPlatform
    return $ if bitfieldPadding then - bs else 0

-- | obtain platform from switchboard
--
getPlatform :: GB PlatformSpec
getPlatform = getSwitch platformSB


-- All this is slightly horrible, but it's the only way to find the
-- size of the C99 _Bool type which is needed for marshalling
-- structures containing C 'bool' values.  (Marshalling of 'bool'
-- function arguments and return values can be done by passing them
-- through the FFI as C 'int', but calculating offsets into structures
-- requires knowledge of the size of the type, which isn't provided by
-- the Haskell FFI.)

{-# NOINLINE cBoolSizeRef #-}
cBoolSizeRef :: IORef (Maybe Int)
cBoolSizeRef = unsafePerformIO $ newIORef Nothing

findBoolSize :: IO Int
findBoolSize = do
  withFile "c2hs__bool_size.c" WriteMode $ \h -> do
    hPutStrLn h "#include <stdio.h>"
    hPutStrLn h $ "int main(int argc, char *argv[]) " ++
      "{ printf(\"%u\\n\", sizeof(_Bool)); return 0; }"
  gcccode <- system $ cCompiler ++ " -o c2hs__bool_size c2hs__bool_size.c"
  when (gcccode /= ExitSuccess) $
    error "Failed to compile 'bool' size test program!"
  (code, stdout, _) <- readProcessWithExitCode "./c2hs__bool_size" [] ""
  when (code /= ExitSuccess) $
    error "Failed to run 'bool' size test program!"
  let sz = read stdout :: Int
  removeFile "c2hs__bool_size.c"
  removeFile "c2hs__bool_size"
  return sz

cBoolSize :: Int
cBoolSize = unsafePerformIO $ do
  msz <- readIORef cBoolSizeRef
  case msz of
    Just sz -> return sz
    Nothing -> do
      sz <- findBoolSize
      writeIORef cBoolSizeRef $ Just sz
      return sz

{-# NOINLINE cCompilerRef #-}
cCompilerRef :: IORef (Maybe String)
cCompilerRef = unsafePerformIO $ newIORef Nothing

cCompiler :: String
cCompiler = unsafePerformIO $ do
  mcc <- readIORef cCompilerRef
  case mcc of
    Just cc -> return cc
    Nothing -> do
      (code, stdout, _) <- readProcessWithExitCode "ghc" ["--info"] ""
      when (code /= ExitSuccess) $
        error "Failed to determine C compiler from 'ghc --info'!"
      let vals = read stdout :: [(String, String)]
      case Prelude.lookup "C compiler command" vals of
        Nothing -> error "Failed to determine C compiler from 'ghc --info'!"
        Just cc -> do
          writeIORef cCompilerRef $ Just cc
          return cc
