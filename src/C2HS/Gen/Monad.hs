{-# OPTIONS_GHC -fno-warn-orphans #-}
--  C->Haskell Compiler: monad for the binding generator
--
--  Author : Manuel M T Chakravarty
--  Derived: 18 February 2 (extracted from GenBind.hs)
--
--  Copyright (c) [2002..2005] Manuel M T Chakravarty
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
--  This modules defines the monad and related utility routines for the code
--  that implements the expansion of the binding hooks.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Translation table handling for enumerators:
--  -------------------------------------------
--
--  First a translation table lookup on the original identifier of the
--  enumerator is done.  If that doesn't match and the prefix can be removed
--  from the identifier, a second lookup on the identifier without the prefix
--  is performed.  If this also doesn't match, the identifier without prefix
--  (possible after underscoreToCase or similar translation is returned).  If
--  there is a match, the translation (without any further stripping of
--  prefix) is returned.
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
--  pointers to this C type that should be mapped as specified.  The pointer
--  map is dumped into and read from `.chi' files.
--
--  Haskell object map
--  ------------------
--
--  Some features require information about Haskell objects defined by c2hs.
--  Therefore, the Haskell object map maintains the necessary information
--  about these Haskell objects.  The Haskell object map is dumped into and
--  read from `.chi' files.
--
--  Enumeration map
--  ---------------
--
--  Map maintaining information about enum hooks for use in generation
--  of default marshalling code.
--
--- TODO ----------------------------------------------------------------------
--
--  * Look up in translation tables is naive - this probably doesn't affect
--    costs much, but at some point a little profiling might be beneficial.
--

module C2HS.Gen.Monad (
  TransFun, transTabToTransFun,

  HsObject(..), Wrapper(..), GB, GBState(..),
  initialGBState, setContext, getLibrary, getPrefix,
  getReplacementPrefix, delayCode, getDelayedCode, ptrMapsTo, queryPtr,
  objIs, queryObj, sizeIs, querySize, queryClass, queryPointer,
  mergeMaps, dumpMaps, queryEnum, isEnum,
  queryTypedef, isC2HSTypedef, queryDefaultMarsh, isDefaultMarsh,
  addWrapper, getWrappers,
  addHsDependency, getHsDependencies
) where

-- standard libraries
import Data.Char  (toUpper, toLower)
import Data.List  (find)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map (empty, insert, lookup, union, toList, fromList)
import Data.Map   (Map)
import Data.Set   (Set)
import qualified Data.Set as Set (empty, insert, member, union, toList, fromList)

-- Language.C
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Syntax
import Data.Errors

-- C -> Haskell
import C2HS.C     (CT, readCT, transCT, raiseErrorCTExc)

-- friends
import C2HS.CHS   (CHSFrag(..), CHSHook(..), CHSTrans(..),
                   CHSChangeCase(..), CHSPtrType(..),
                   CHSTypedefInfo, CHSDefaultMarsh, Direction(..))


-- translation tables
-- ------------------

-- | takes an identifier to a lexeme including a potential mapping by a
-- translation table
--
type TransFun = Ident -> Maybe String

-- | translation function for the 'underscoreToCase' flag
--
underscoreToCase :: String -> String
underscoreToCase lexeme =
  let
    ps = filter (not . null) . parts $ lexeme
  in
  concat . map adjustCase $ ps
  where
    parts s = let (l, s') = break (== '_') s
              in
              l : case s' of
                    []      -> []
                    (_:s'') -> parts s''

    adjustCase (c:cs) = toUpper c : map toLower cs

-- | translation function for the 'upcaseFirstLetter' flag
--
upcaseFirstLetter :: String -> String
upcaseFirstLetter ""     = ""
upcaseFirstLetter (c:cs) = toUpper c : cs

-- | translation function for the 'downcaseFirstLetter' flag
--
downcaseFirstLetter :: String -> String
downcaseFirstLetter ""     = ""
downcaseFirstLetter (c:cs) = toLower c : cs

-- | takes an identifier association table to a translation function
--
-- * if first argument is 'True', identifiers that are not found in the
--   translation table are subjected to 'underscoreToCase' and friends
--
-- * the details of handling the prefix are given in the DOCU section at the
--   beginning of this file
--
transTabToTransFun :: String -> String -> CHSTrans -> TransFun
transTabToTransFun prefx rprefx (CHSTrans _2Case chgCase table omits) =
  \ide ->
  let caseTrafo = (if _2Case then underscoreToCase else id) .
                  (case chgCase of
                      CHSSameCase -> id
                      CHSUpCase   -> upcaseFirstLetter
                      CHSDownCase -> downcaseFirstLetter)
      lexeme = identToString ide
      dft    = caseTrafo lexeme             -- default uses case trafo
  in if ide `elem` omits
     then Nothing
     else Just $
          case lookup ide table of                  -- lookup original ident
            Just ide' -> identToString ide'         -- original ident matches
            Nothing   ->
              case eat prefx lexeme of
                Nothing          -> dft             -- no match & no prefix
                Just eatenLexeme ->
                  let
                    eatenIde = internalIdentAt (posOf ide)
                               (rprefx ++ eatenLexeme)
                    eatenDft = caseTrafo rprefx ++ caseTrafo eatenLexeme
                  in
                  case lookup eatenIde table of     -- lookup without prefix
                    Nothing   -> eatenDft           -- orig ide without prefix
                    Just ide' -> identToString ide' -- without prefix matched
  where
    -- try to eat prefix and return `Just partialLexeme' if successful
    --
    eat []         ('_':cs)                         = eat [] cs
    eat []         cs                               = Just cs
    eat (p:prefx') (c:cs) | toUpper p == toUpper c  = eat prefx' cs
                          | otherwise              = Nothing
    eat _          _                               = Nothing


-- the local monad
-- ---------------

-- | map that for maps C pointer types to Haskell types for pointer that have
-- been registered using a pointer hook
--
-- * the 'Bool' indicates whether for a C type "ctype", we map "ctype" itself
--   or "*ctype"
--
-- * in the co-domain, the first string is the type for function arguments and
--   the second string is for function results; this distinction is necessary
--   as 'ForeignPtr's cannot be returned by a foreign function; the
--   restriction on function result types is only for the actual result, not
--   for type arguments to parametrised pointer types, ie, it holds for @res@
--   in `Int -> IO res', but not in `Int -> Ptr res'
--
type PointerMap = Map (Bool, Ident) (String, String)

-- | map that maintains key information about some of the Haskell objects
-- generated by c2hs
--
-- NB: using records here avoids to run into a bug with deriving 'Read' in GHC
--     5.04.1
--
data HsObject    = Pointer {
                     ptrTypeHO    :: CHSPtrType,   -- kind of pointer
                     isNewtypeHO  :: Bool          -- newtype?
                   }
                 | Class {
                     superclassHO :: (Maybe Ident),-- superclass
                     ptrHO        :: Ident         -- pointer
                   }
                 deriving (Show, Read)
type HsObjectMap = Map Ident HsObject

type SizeMap = Map Ident Int

-- | set of Haskell type names corresponding to C enums.
type EnumSet = Set String

-- Map from C type names to type default definitions.
type TypedefMap = Map Ident CHSTypedefInfo

-- Map from C type names to type default definitions.
type DefaultMarshMap = Map (Direction, String, Bool) CHSDefaultMarsh

-- Definitions for bare structure function wrappers.
data Wrapper = Wrapper { wrapFn :: String
                       , wrapOrigFn ::String
                       , wrapDecl :: CDecl
                       , wrapArgs :: [Bool]
                       , wrapBools :: (Bool, [Bool])
                       , wrapPos :: Position }
             deriving Show

instance Eq Wrapper where
  w1 == w2 = wrapFn w1 == wrapFn w2

instance Ord Wrapper where
  compare w1 w2 = compare (wrapFn w1) (wrapFn w2)

type WrapperSet = Set Wrapper

type Dependencies = Set String

{- FIXME: What a mess...
instance Show HsObject where
  show (Pointer ptrType isNewtype) =
    "Pointer " ++ show ptrType ++ show isNewtype
  show (Class   osuper  pointer  ) =
    "Class " ++ show ptrType ++ show isNewtype
-}

-- Remove everything until the next element in the list (given by a
-- ","), the end of the list (marked by "]"), or the end of a record
-- "}". Everything inside parenthesis is ignored.
chopIdent :: String -> String
chopIdent str = goChop 0 str
    where goChop :: Int -> String -> String
          goChop 0 rest@('}':_) = rest
          goChop 0 rest@(',':_) = rest
          goChop 0 rest@(']':_) = rest
          goChop level ('(':rest) = goChop (level+1) rest
          goChop level (')':rest) = goChop (level-1) rest
          goChop level (_  :rest) = goChop level rest
          goChop _     [] = []

extractIdent :: String -> (Ident, String)
extractIdent str =
    let isQuote c = c == '\'' || c == '"'
        (ideChars, rest) = span (not . isQuote)
                           . tail
                           . dropWhile (not . isQuote) $ str
    in
      if null ideChars
      then error $ "Could not interpret " ++ show str ++ "as an Ident."
      else (internalIdent ideChars, (chopIdent . tail) rest)

-- super kludgy (depends on Show instance of Ident)
instance Read Ident where
  readsPrec _ str = [extractIdent str]

-- | the local state consists of
--
-- (1) the dynamic library specified by the context hook,
-- (2) the prefix specified by the context hook,
-- (3) the set of delayed code fragaments, ie, pieces of Haskell code that,
--     finally, have to be appended at the CHS module together with the hook
--     that created them (the latter allows avoid duplication of foreign
--     export declarations), and
-- (4) a map associating C pointer types with their Haskell representation
--
-- access to the attributes of the C structure tree is via the 'CT' monad of
-- which we use an instance here
--
data GBState  = GBState {
  lib       :: String,               -- dynamic library
  prefix    :: String,               -- prefix
  repprefix :: String,               -- replacement prefix
  frags     :: [(CHSHook, CHSFrag)], -- delayed code (with hooks)
  ptrmap    :: PointerMap,           -- pointer representation
  objmap    :: HsObjectMap,          -- generated Haskell objects
  szmap     :: SizeMap,              -- object sizes
  enums     :: EnumSet,              -- enumeration hooks
  tdmap     :: TypedefMap,           -- typedefs
  dmmap     :: DefaultMarshMap,      -- user-defined default marshallers
  wrappers  :: WrapperSet,           -- C wrapper functions
  deps      :: Dependencies          -- Haskell dependencies (for imports)
  }

type GB a = CT GBState a

initialGBState :: GBState
initialGBState  = GBState {
                    lib    = "",
                    prefix = "",
                    repprefix = "",
                    frags  = [],
                    ptrmap = Map.empty,
                    objmap = Map.empty,
                    szmap = Map.empty,
                    enums = Set.empty,
                    tdmap = Map.empty,
                    dmmap = Map.empty,
                    wrappers = Set.empty,
                    deps = Set.empty
                  }

-- | set the dynamic library and library prefix
--
setContext :: (Maybe String) -> (Maybe String) -> (Maybe String) -> GB ()
setContext lib' prefix' repprefix' =
  transCT $ \state -> (state {lib       = fromMaybe "" lib',
                              prefix    = fromMaybe "" prefix',
                              repprefix = fromMaybe "" repprefix'},
                       ())

-- | get the dynamic library
--
getLibrary :: GB String
getLibrary  = readCT lib

-- | get the prefix string
--
getPrefix :: GB String
getPrefix  = readCT prefix

-- | get the replacement prefix string
--
getReplacementPrefix :: GB String
getReplacementPrefix  = readCT repprefix

-- | add code to the delayed fragments (the code is made to start at a new line)
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
    frags'' <- readCT frags
    frags'  <- delay hook frags''
    transCT (\state -> (state {frags = frags'}, ()))
    where
      newEntry = (hook, (CHSVerb ("\n" ++ str) (posOf hook)))
      --
      delay hook'@(CHSCall isFun isUns ide _oalias _) frags' =
        case find (\(hook'', _) -> hook'' == hook') frags' of
          Just (CHSCall isFun' isUns' ide' _ _, _)
            |    isFun == isFun'
              && isUns == isUns'
              && ide   == ide'   -> return frags'
            | otherwise          -> err (posOf ide) (posOf ide')
          Nothing                -> return $ frags' ++ [newEntry]
      delay hook'@(CHSPointer _ _ _ _ _ _ _ _) frags' =
        case find (\(hook'', _) -> hook'' == hook') frags' of
          Just (CHSPointer _ _ _ _ _ _ _ _, _) -> return frags'
          Nothing                              -> return $ frags' ++ [newEntry]
      delay _ _                                  =
        interr "GBMonad.delayCode: Illegal delay!"
      --
      err = incompatibleCallHooksErr

-- | get the complete list of delayed fragments
--
getDelayedCode :: GB [CHSFrag]
getDelayedCode  = readCT (map snd . frags)

-- | add an entry to the pointer map
--
ptrMapsTo :: (Bool, Ident) -> (String, String) -> GB ()
(isStar, cName) `ptrMapsTo` hsRepr =
  transCT (\state -> (state {
                        ptrmap = Map.insert (isStar, cName) hsRepr (ptrmap state)
                      }, ()))

-- | query the pointer map
--
queryPtr        :: (Bool, Ident) -> GB (Maybe (String, String))
queryPtr pcName  = do
                     fm <- readCT ptrmap
                     return $ Map.lookup pcName fm

-- | add an entry to the Haskell object map
--
objIs :: Ident -> HsObject -> GB ()
hsName `objIs` obj =
  transCT (\state -> (state {
                        objmap = Map.insert hsName obj (objmap state)
                      }, ()))

-- | query the Haskell object map
--
queryObj        :: Ident -> GB (Maybe HsObject)
queryObj hsName  = do
                     fm <- readCT objmap
                     return $ Map.lookup hsName fm

-- | add an entry to the size map
--
sizeIs :: Ident -> Int -> GB ()
hsName `sizeIs` sz =
  transCT (\state -> (state {
                        szmap = Map.insert hsName sz (szmap state)
                      }, ()))

-- | query the size map
--
querySize       :: Ident -> GB (Maybe Int)
querySize hsName  = do
                     sm <- readCT szmap
                     return $ Map.lookup hsName sm

-- | query the Haskell object map for a class
--
-- * raise an error if the class cannot be found
--
queryClass        :: Ident -> GB HsObject
queryClass hsName  = do
                       oobj <- queryObj hsName
                       case oobj of
                         Just obj@(Class _ _) -> return obj
                         Just _               -> classExpectedErr hsName
                         Nothing              -> hsObjExpectedErr hsName

-- | query the Haskell object map for a pointer
--
-- * raise an error if the pointer cannot be found
--
queryPointer        :: Ident -> GB HsObject
queryPointer hsName  = do
                       oobj <- queryObj hsName
                       case oobj of
                         Just obj@(Pointer _ _) -> return obj
                         Just _                 -> pointerExpectedErr hsName
                         Nothing                -> hsObjExpectedErr hsName

-- | merge the pointer and Haskell object maps
--
-- * currently, the read map overrides any entires for shared keys in the map
--   that is already in the monad; this is so that, if multiple import hooks
--   add entries for shared keys, the textually latest prevails; any local
--   entries are entered after all import hooks anyway
--
-- FIXME: This currently has several shortcomings:
--        * It just dies in case of a corrupted .chi file
--        * We should at least have the option to raise a warning if two
--          entries collide in the 'objmap'.  But it would be better to
--          implement qualified names.
--        * Do we want position information associated with the read idents?
--
mergeMaps     :: String -> GB ()
mergeMaps str  =
  transCT (\state -> (state {
                        ptrmap = Map.union readPtrMap (ptrmap state),
                        objmap = Map.union readObjMap (objmap state),
                        enums = Set.union readEnumSet (enums state)
                      }, ()))
  where
    -- Deal with variant interface file formats (old .chi files don't
    -- contain the list of enumerations).
    (ptrAssoc, objAssoc, enumList) =
      case reads str of
        [] -> let (ptr, obj) = read str in (ptr, obj, [])
        [(r, "")] -> r
    readPtrMap           = Map.fromList [((isStar, internalIdent ide), repr)
                                        | ((isStar, ide), repr) <- ptrAssoc]
    readObjMap           = Map.fromList [(internalIdent ide, obj)
                                        | (ide, obj)            <- objAssoc]
    readEnumSet          = Set.fromList enumList

-- | convert the whole pointer and Haskell object maps into printable form
--
dumpMaps :: GB String
dumpMaps  = do
              ptrFM <- readCT ptrmap
              objFM <- readCT objmap
              enumS <- readCT enums
              let dumpable = ([((isStar, identToString ide), repr)
                              | ((isStar, ide), repr) <- Map.toList ptrFM],
                              [(identToString ide, obj)
                              | (ide, obj)            <- Map.toList objFM],
                              Set.toList enumS)
              return $ show dumpable

-- | query the enum map
--
queryEnum :: String -> GB Bool
queryEnum hsName  = do
  es <- readCT enums
  return $ hsName `Set.member` es

-- | add an entry to the enum map
--
isEnum :: String -> GB ()
isEnum hsName =
  transCT (\state -> (state { enums = Set.insert hsName (enums state) }, ()))

-- | query the type default map
--
queryTypedef :: Ident -> GB (Maybe CHSTypedefInfo)
queryTypedef cIde  = do
  tds <- readCT tdmap
  return $ cIde `Map.lookup` tds

-- | add an entry to the type default map
--
isC2HSTypedef :: Ident -> CHSTypedefInfo -> GB ()
isC2HSTypedef cIde td =
  transCT (\state -> (state { tdmap = Map.insert cIde td (tdmap state) }, ()))

-- | query the default marshaller map
--
queryDefaultMarsh :: (Direction, String, Bool) -> GB (Maybe CHSDefaultMarsh)
queryDefaultMarsh k  = do
  dms <- readCT dmmap
  return $ k `Map.lookup` dms

-- | add an entry to the type default map
--
isDefaultMarsh :: (Direction, String, Bool) -> CHSDefaultMarsh -> GB ()
isDefaultMarsh k dm =
  transCT (\state -> (state { dmmap = Map.insert k dm (dmmap state) }, ()))

-- | add a wrapper definition
addWrapper :: String -> String -> CDecl ->
              [Bool] -> (Bool, [Bool]) -> Position -> GB ()
addWrapper wfn ofn cdecl args bools pos =
  let w = Wrapper wfn ofn cdecl args bools pos
  in transCT (\st -> (st { wrappers = Set.insert w (wrappers st) }, ()))

getWrappers :: GB [Wrapper]
getWrappers = Set.toList `fmap` readCT wrappers


-- | add Haskell module dependency for import generation
addHsDependency :: String -> GB ()
addHsDependency m = transCT (\st -> (st { deps = Set.insert m (deps st) }, ()))

getHsDependencies :: GB [String]
getHsDependencies = Set.toList `fmap` readCT deps


-- error messages
-- --------------

incompatibleCallHooksErr            :: Position -> Position -> GB a
incompatibleCallHooksErr here there  =
  raiseErrorCTExc here
    ["Incompatible call hooks!",
     "There is a another call hook for the same C function at " ++ show there,
     "The flags and C function name of the two hooks should be identical,",
     "but they are not."]

classExpectedErr     :: Ident -> GB a
classExpectedErr ide  =
  raiseErrorCTExc (posOf ide)
    ["Expected a class name!",
     "Expected `" ++ identToString ide ++ "' to refer to a class introduced",
     "by a class hook."]

pointerExpectedErr     :: Ident -> GB a
pointerExpectedErr ide  =
  raiseErrorCTExc (posOf ide)
    ["Expected a pointer name!",
     "Expected `" ++ identToString ide ++ "' to be a type name introduced by",
     "a pointer hook."]

hsObjExpectedErr     :: Ident -> GB a
hsObjExpectedErr ide  =
  raiseErrorCTExc (posOf ide)
    ["Unknown name!",
     "`" ++ identToString ide ++ "' is unknown; it has *not* been defined by",
     "a previous hook."]
