{-# LANGUAGE StandaloneDeriving #-}
--  C->Haskell Compiler: CHS file abstraction
--
--  Author : Manuel M T Chakravarty
--  Created: 16 August 99
--
--  Copyright (c) [1999..2005] Manuel M T Chakravarty
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
--  Main file for reading CHS files.
--
--  Import hooks & .chi files
--  -------------------------
--
--  Reading of `.chi' files is interleaved with parsing.  More precisely,
--  whenever the parser comes across an import hook, it immediately reads the
--  `.chi' file and inserts its contents into the abstract representation of
--  the hook.  The parser checks the version of the `.chi' file, but does not
--  otherwise attempt to interpret its contents.  This is only done during
--  generation of the binding module.  The first line of a .chi file has the
--  form
--
--    C->Haskell Interface Version <version>
--
--  where <version> is the three component version number `Version.version'.
--  C->Haskell will only accept files whose version number match its own in
--  the first two components (ie, major and minor version).  In other words,
--  it must be guaranteed that the format of .chi files is not altered between
--  versions that differ only in their patchlevel.  All remaining lines of the
--  file are version dependent and contain a dump of state information that
--  the binding file generator needs to rescue across modules.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The following binding hooks are recognised:
--
--  hook     -> `{#' inner `#}'
--  inner    -> `import' ['qualified'] ident
--            | `context' ctxt
--            | `type' ident
--            | `sizeof' ident
--            | `alignof' ident
--            | `enum' idalias trans [`nocode'] [`with' prefix] [`add' prefix] [deriving]
--            | `enum` `define` idalias [deriving]
--            | `call' [`pure'] [`unsafe'] idalias
--            | `fun' [`pure'] [`unsafe'] idalias parms
--            | `get' [`struct'] apath
--            | `set' [`struct'] apath
--            | `offsetof` apath
--            | `pointer' ['*'] idalias ptrkind ['nocode']
--            | `class' [ident `=>'] ident ident
--            | `const' ident
--            | `default' ident ident [dft ...]
--  ctxt     -> [`lib' `=' string] [prefix]
--  idalias  -> ident
--            | looseident [`as' (ident | `^' | `'' ident1 ident2 ... `'')]
--  prefix   -> `prefix' `=' string [`add' `prefix' `=' string]
--  deriving -> `deriving' `(' ident_1 `,' ... `,' ident_n `)'
--  parms    -> [verbhs `=>'] `{' parm_1 `,' ... `,' parm_n `}' `->' parm
--  parm     -> `+'
--            | [ident_or_quot_1 [`*' | `-']] verbhs [`&'] [ident_or_quot_2 [`*'] [`-']]
--  ident_or_quot -> ident | quoths
--  apath    -> ident
--            | `*' apath
--            | apath `.' ident
--            | apath `->' ident
--  trans    -> `{' alias_1 `,' ... `,' alias_n `}' [omit]
--  omit     -> `omit' `(' ident_1 `,' ... `,' ident_n `)'
--  alias    -> `underscoreToCase' | `upcaseFirstLetter'
--            | `downcaseFirstLetter'
--            | ident `as' ident
--  ptrkind  -> [`foreign' [`finalizer' idalias] | `stable'] ['newtype' | '->' ident]
--  dft      -> dfttype `=` ident [`*']
--  dfttype  -> `in' | `out' | `ptr_in' | `ptr_out'
--
--  If `underscoreToCase', `upcaseFirstLetter', or `downcaseFirstLetter'
--  occurs in a translation table, it must be the first entry, or if two of
--  them occur the first two entries.
--
--  Remark: Optional Haskell names are normalised during structure tree
--          construction, ie, associations that associated a name with itself
--          are removed.  (They don't carry semantic content, and make some
--          tests more complicated.)
--
--- TODO ----------------------------------------------------------------------
--

module C2HS.CHS (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..),
            CHSChangeCase(..), CHSParm(..), CHSMarsh, CHSArg(..), CHSAccess(..),
            CHSAPath(..), CHSPtrType(..), CHSTypedefInfo, CHSDefaultMarsh,
            Direction(..),
            loadCHS, dumpCHS, hssuffix, chssuffix, loadCHI, dumpCHI, chisuffix,
            showCHSParm, apathToIdent, apathRootIdent, hasNonGNU,
            isParmWrapped)
where

-- standard libraries
import Data.Char (isSpace, toUpper, toLower)
import Data.List (intersperse)
import Control.Monad (when)
import System.FilePath ((<.>), (</>))


-- Language.C
import Language.C.Data.Ident
import Language.C.Data.Position
import Data.Errors       (interr)

-- C->Haskell
import C2HS.State (CST, getSwitch, chiPathSB, catchExc, throwExc, raiseError,
                  fatal, errorsPresent, showErrors, Traces(..), putTraceStr)
import qualified System.CIO as CIO
import C2HS.C.Info (CPrimType(..))
import C2HS.Version    (version)

-- friends
import C2HS.CHS.Lexer  (CHSToken(..), lexCHS, keywordToIdent)


-- CHS abstract syntax
-- -------------------

-- | representation of a CHS module
--
data CHSModule = CHSModule [CHSFrag]

deriving instance Show CHSModule
deriving instance Show CHSFrag
deriving instance Show CHSHook
deriving instance Show CHSAccess
deriving instance Show CHSParm
deriving instance Show CHSTrans
deriving instance Show CHSArg
deriving instance Show CHSChangeCase

-- | a CHS code fragament
--
-- * 'CHSVerb' fragments are present throughout the compilation and finally
--   they are the only type of fragment (describing the generated Haskell
--   code)
--
-- * 'CHSHook' are binding hooks, which are being replaced by Haskell code by
--   'GenBind.expandHooks'
--
-- * 'CHSCPP' and 'CHSC' are fragements of C code that are being removed when
--   generating the custom C header in 'GenHeader.genHeader'
--
-- * 'CHSCond' are strutured conditionals that are being generated by
--   'GenHeader.genHeader' from conditional CPP directives ('CHSCPP')
--
data CHSFrag = CHSVerb String                   -- Haskell code
                       Position
             | CHSHook CHSHook                  -- binding hook
                       Position
             | CHSCPP  String                   -- pre-processor directive
                       Position
                       Bool
             | CHSLine Position                 -- line pragma
             | CHSC    String                   -- C code
                       Position
             | CHSCond [(Ident,                 -- C variable repr. condition
                         [CHSFrag])]            -- then/elif branches
                       (Maybe [CHSFrag])        -- else branch

hasNonGNU :: CHSModule -> Bool
hasNonGNU (CHSModule frags) = any isNonGNU frags
  where isNonGNU (CHSHook (CHSNonGNU _) _) = True
        isNonGNU _                         = False


instance Pos CHSFrag where
  posOf (CHSVerb _ pos  ) = pos
  posOf (CHSHook _ pos  ) = pos
  posOf (CHSCPP  _ pos _) = pos
  posOf (CHSLine   pos  ) = pos
  posOf (CHSC    _ pos  ) = pos
  posOf (CHSCond alts _ ) = case alts of
                             (_, frag:_):_ -> posOf frag
                             _             -> nopos

-- | a CHS binding hook
--
data CHSHook = CHSImport  Bool                  -- qualified?
                          Ident                 -- module name
                          String                -- content of .chi file
                          Position
             | CHSContext (Maybe String)        -- library name
                          (Maybe String)        -- prefix
                          (Maybe String)        -- replacement prefix
                          Position
             | CHSNonGNU  Position
             | CHSType    Ident                 -- C type
                          Position
             | CHSSizeof  Ident                 -- C type
                          Position
             | CHSAlignof Ident                 -- C type
                          Position
             | CHSEnum    Ident                 -- C enumeration type
                          (Maybe Ident)         -- Haskell name
                          CHSTrans              -- translation table
                          Bool                  -- emit code or not?
                          (Maybe String)        -- local prefix
                          (Maybe String)        -- local replacement prefix
                          [Ident]               -- instance requests from user
                          Position
             | CHSEnumDefine Ident              -- Haskell name
                          CHSTrans              -- translation table
                          [Ident]               -- instance requests from user
                          Position
             | CHSCall    Bool                  -- is a pure function?
                          Bool                  -- is unsafe?
                          CHSAPath              -- C function
                          (Maybe Ident)         -- Haskell name
                          Position
             | CHSFun     Bool                  -- is a pure function?
                          Bool                  -- is unsafe?
                          Bool                  -- is variadic?
                          [String]              -- variadic C parameter types
                          CHSAPath              -- C function
                          (Maybe Ident)         -- Haskell name
                          (Maybe String)        -- type context
                          [CHSParm]             -- argument marshalling
                          CHSParm               -- result marshalling
                          Position
             | CHSField   CHSAccess             -- access type
                          CHSAPath              -- access path
                          Position
             | CHSOffsetof CHSAPath             -- access path
                           Position
             | CHSPointer Bool                  -- explicit '*' in hook
                          Ident                 -- C pointer name
                          (Maybe Ident)         -- Haskell name
                          CHSPtrType            -- Ptr, ForeignPtr or StablePtr
                          Bool                  -- create new type?
                          [Ident]               -- Haskell type pointed to
                          Bool                  -- emit type decl?
                          Position
             | CHSClass   (Maybe Ident)         -- superclass
                          Ident                 -- class name
                          Ident                 -- name of pointer type
                          Position
             | CHSConst   Ident                 -- C identifier
                          Position
             | CHSTypedef Ident                 -- C type name
                          Ident                 -- Haskell type name
                          Position
             | CHSDefault Direction             -- in or out marshaller?
                          String                -- Haskell type name
                          String                -- C type string
                          Bool                  -- is it a C pointer?
                          (Either Ident String, CHSArg) -- marshaller
                          Position

data Direction = In | Out deriving (Eq, Ord, Show)

instance Pos CHSHook where
  posOf (CHSImport  _ _ _             pos) = pos
  posOf (CHSContext _ _ _             pos) = pos
  posOf (CHSType    _                 pos) = pos
  posOf (CHSSizeof  _                 pos) = pos
  posOf (CHSAlignof _                 pos) = pos
  posOf (CHSEnum    _ _ _ _ _ _ _     pos) = pos
  posOf (CHSEnumDefine _ _ _          pos) = pos
  posOf (CHSCall    _ _ _ _           pos) = pos
  posOf (CHSFun     _ _ _ _ _ _ _ _ _ pos) = pos
  posOf (CHSField   _ _               pos) = pos
  posOf (CHSOffsetof _                pos) = pos
  posOf (CHSPointer _ _ _ _ _ _ _     pos) = pos
  posOf (CHSClass   _ _ _             pos) = pos
  posOf (CHSConst   _                 pos) = pos
  posOf (CHSTypedef _ _               pos) = pos
  posOf (CHSDefault _ _ _ _ _         pos) = pos

-- | two hooks are equal if they have the same Haskell name and reference the
-- same C object
--
instance Eq CHSHook where
  (CHSImport qual1 ide1 _           _) == (CHSImport qual2 ide2 _           _) =
    qual1 == qual2 && ide1 == ide2
  (CHSContext olib1 opref1 orpref1  _) == (CHSContext olib2 opref2 orpref2  _) =
    olib1 == olib2 && opref1 == opref2 && orpref1 == orpref2
  (CHSType ide1                     _) == (CHSType ide2                     _) =
    ide1 == ide2
  (CHSSizeof ide1                   _) == (CHSSizeof ide2                   _) =
    ide1 == ide2
  (CHSAlignof ide1                  _) == (CHSAlignof ide2                  _) =
    ide1 == ide2
  (CHSEnum ide1 oalias1 _ _ _ _ _   _) == (CHSEnum ide2 oalias2 _ _ _ _ _   _) =
    oalias1 == oalias2 && ide1 == ide2
  (CHSEnumDefine ide1 _ _           _) == (CHSEnumDefine ide2 _ _           _) =
    ide1 == ide2
  (CHSCall _ _ ide1 oalias1         _) == (CHSCall _ _ ide2 oalias2         _) =
    oalias1 == oalias2 && ide1 == ide2
  (CHSFun _ _ _ _ ide1 oalias1 _ _ _ _) ==
    (CHSFun _ _ _ _ ide2 oalias2 _ _ _ _) = oalias1 == oalias2 && ide1 == ide2
  (CHSField acc1 path1              _) == (CHSField acc2 path2              _) =
    acc1 == acc2 && path1 == path2
  (CHSOffsetof path1                _) == (CHSOffsetof path2                _) =
    path1 == path2
  (CHSPointer _ ide1 oalias1 _ _ _ _ _)
                                      == (CHSPointer _ ide2 oalias2 _ _ _ _ _) =
    ide1 == ide2 && oalias1 == oalias2
  (CHSClass _ ide1 _                _) == (CHSClass _ ide2 _                _) =
    ide1 == ide2
  (CHSConst ide1                    _) == (CHSConst ide2                    _) =
    ide1 == ide2
  (CHSTypedef ide1 _                _) == (CHSTypedef ide2 _                _) =
    ide1 == ide2
  (CHSDefault _ ide1 _ _ _          _) == (CHSDefault _ ide2 _ _ _          _) =
    ide1 == ide2
  _                               == _                          = False

-- | translation table
--
data CHSTrans = CHSTrans Bool                   -- underscore to case?
                         CHSChangeCase          -- upcase or downcase?
                         [(Ident, Ident)]       -- alias list
                         [Ident]                -- omit list

data CHSChangeCase = CHSSameCase
                   | CHSUpCase
                   | CHSDownCase
                   deriving Eq

-- | marshaller consists of a function name or verbatim Haskell code
--   and flag indicating whether it has to be executed in the IO monad
--
type CHSMarsh = Maybe (Either Ident String, CHSArg)

-- | Type default information
type CHSTypedefInfo = (Ident, CPrimType)

-- | Type default information
type CHSDefaultMarsh = (Either Ident String, CHSArg)

-- | marshalling descriptor for function hooks
--
data CHSParm = CHSPlusParm       -- special "+" parameter
             | CHSParm CHSMarsh  -- "in" marshaller
                       String    -- Haskell type
                       Bool      -- C repr: two values?
                       CHSMarsh  -- "out" marshaller
                       Bool      -- wrapped?
                       Position
                       String    -- Comment for this para

-- | Check whether parameter requires wrapping for bare structures.
--
isParmWrapped :: CHSParm -> Bool
isParmWrapped (CHSParm _ _ _ _ w _ _) = w
isParmWrapped _ = False

-- | kinds of arguments in function hooks
--
data CHSArg = CHSValArg                         -- plain value argument
            | CHSIOArg                          -- reference argument
            | CHSVoidArg                        -- no argument
            | CHSIOVoidArg                      -- drops argument, but in monad
            deriving (Eq)

-- | structure member access types
--
data CHSAccess = CHSSet                         -- set structure field
               | CHSGet                         -- get structure field
               deriving (Eq)

-- | structure access path
--
data CHSAPath = CHSRoot Bool Ident
                -- root of access path with flag indicating presence
                -- of "struct" keyword
              | CHSDeref CHSAPath Position      -- dereferencing
              | CHSRef   CHSAPath Ident         -- member referencing
              deriving (Eq,Show)

instance Pos CHSAPath where
    posOf (CHSRoot _ ide)  = posOf ide
    posOf (CHSDeref _ pos) = pos
    posOf (CHSRef _ ide)   = posOf ide

-- | pointer options
--

data CHSPtrType = CHSPtr
                  -- standard Ptr from Haskell
                | CHSForeignPtr (Maybe (Ident, Maybe Ident))
                  -- a foreign pointer possibly with a finalizer
                | CHSStablePtr
                  -- a pointer into Haskell land
                deriving (Eq)

instance Show CHSPtrType where
  show CHSPtr            = "Ptr"
  show (CHSForeignPtr _) = "ForeignPtr"
  show CHSStablePtr      = "StablePtr"

instance Read CHSPtrType where
  readsPrec _ (                            'P':'t':'r':rest) =
    [(CHSPtr, rest)]
  readsPrec _ ('F':'o':'r':'e':'i':'g':'n':'P':'t':'r':rest) =
    [(CHSForeignPtr Nothing, rest)]
  readsPrec _ ('S':'t':'a':'b':'l':'e'    :'P':'t':'r':rest) =
    [(CHSStablePtr, rest)]
  readsPrec p (c:cs)
    | isSpace c                                              = readsPrec p cs
  readsPrec _ _                                              = []


-- load and dump a CHS file
-- ------------------------

hssuffix, chssuffix :: String
hssuffix  = "hs"
chssuffix = "chs"

-- | load a CHS module
--
-- * the file suffix is automagically appended
--
-- * in case of a syntactical or lexical error, a fatal error is raised;
--   warnings are returned together with the module
--
loadCHS       :: FilePath -> CST s (CHSModule, String)
loadCHS fname  = do
                   let fullname = fname <.> chssuffix

                   -- read file
                   --
                   traceInfoRead fullname
                   contents <- CIO.readFile fullname

                   -- parse
                   --
                   traceInfoParse
                   mod' <- parseCHSModule (initPos fullname) contents

                   -- check for errors and finalize
                   --
                   errs <- errorsPresent
                   if errs
                     then do
                       traceInfoErr
                       errmsgs <- showErrors
                       fatal ("CHS module contains \
                              \errors:\n\n" ++ errmsgs)   -- fatal error
                     else do
                       traceInfoOK
                       warnmsgs <- showErrors
                       return (mod', warnmsgs)
                  where
                    traceInfoRead fname' = putTraceStr tracePhasesSW
                                             ("Attempting to read file `"
                                              ++ fname' ++ "'...\n")
                    traceInfoParse       = putTraceStr tracePhasesSW
                                             ("...parsing `"
                                              ++ fname ++ "'...\n")
                    traceInfoErr         = putTraceStr tracePhasesSW
                                             ("...error(s) detected in `"
                                              ++ fname ++ "'.\n")
                    traceInfoOK          = putTraceStr tracePhasesSW
                                             ("...successfully loaded `"
                                              ++ fname ++ "'.\n")

-- | given a file name (no suffix) and a CHS module, the module is printed
-- into that file
--
-- * the module can be flagged as being pure Haskell
--
-- * the correct suffix will automagically be appended
--
dumpCHS                       :: String -> CHSModule -> Bool -> CST s ()
dumpCHS fname mod' pureHaskell  =
  do
    let (suffix, kind) = if pureHaskell
                         then (hssuffix , "(Haskell)")
                         else (chssuffix, "(C->HS binding)")
    CIO.writeFile (fname <.> suffix) (contents version kind)
  where
    contents version' kind =
      "-- GENERATED by " ++ version' ++ " " ++ kind ++ "\n\
      \-- Edit the ORIGNAL .chs file instead!\n\n"
      ++ showCHSModule mod' pureHaskell

-- | to keep track of the current state of the line emission automaton
--
data LineState = Emit           -- emit LINE pragma if next frag is Haskell
               | Wait           -- emit LINE pragma after the next '\n'
               | NoLine         -- no pragma needed
               deriving (Eq)

-- | convert a CHS module into a string
--
-- * if the second argument is 'True', all fragments must contain Haskell code
--
showCHSModule                               :: CHSModule -> Bool -> String
showCHSModule (CHSModule fragments) pureHaskell  =
  showFrags pureHaskell Emit fragments []
  where
    -- the second argument indicates whether the next fragment (if it is
    -- Haskell code) should be preceded by a LINE pragma; in particular
    -- generated fragments and those following them need to be prefixed with a
    -- LINE pragma
    --
    showFrags :: Bool -> LineState -> [CHSFrag] -> ShowS
    showFrags _      _     []                           = id
    showFrags pureHs state (CHSVerb s      pos : frags) =
      let
        (fname,line)     = (posFile pos, posRow pos)
        generated        = isBuiltinPos pos
        emitNow          = state == Emit ||
                           (state == Wait && not (null s) && head s == '\n')
        nextState        = if generated then Wait else NoLine
      in
        (if emitNow then
           showString ("\n{-# LINE " ++ show (line `max` 0) ++ " " ++
                       show fname ++ " #-}\n")
         else id)
      . showString s
      . showFrags pureHs nextState frags
    showFrags False  _     (CHSHook hook _     : frags) =
        showString "{#"
      . showCHSHook hook
      . showString "#}"
      . showFrags False Wait frags
    showFrags False  _     (CHSCPP  s    _ nl  : frags) =
      (if nl then showChar '\n' else id)
      . showChar '#'
      . showString s
      . showFrags False Emit frags
    showFrags pureHs _     (CHSLine _s         : frags) =
        showFrags pureHs Emit frags
    showFrags False  _     (CHSC    s    _     : frags) =
        showString "\n#c"
      . showString s
      . showString "\n#endc"
      . showFrags False Emit frags
    showFrags False  _     (CHSCond _    _     : _    ) =
      interr "showCHSFrag: Cannot print `CHSCond'!"
    showFrags True   _     _                            =
      interr "showCHSFrag: Illegal hook, cpp directive, or inline C code!"

showCHSHook :: CHSHook -> ShowS
showCHSHook (CHSImport isQual ide _ _) =
    showString "import "
  . (if isQual then showString "qualified " else id)
  . showCHSIdent ide
showCHSHook (CHSContext olib oprefix oreplprefix _) =
    showString "context "
  . (case olib of
       Nothing  -> showString ""
       Just lib -> showString "lib = " . showString lib . showString " ")
  . showPrefix oprefix False
  . showReplacementPrefix oreplprefix
showCHSHook (CHSType ide _) =
    showString "type "
  . showCHSIdent ide
showCHSHook (CHSSizeof ide _) =
    showString "sizeof "
  . showCHSIdent ide
showCHSHook (CHSAlignof   ide _) =
   showString "alignof "
  . showCHSIdent ide
showCHSHook (CHSEnum ide oalias trans emit oprefix oreplprefix derive _) =
    showString "enum "
  . showIdAlias ide oalias
  . showCHSTrans trans
  . (case emit of
        True  -> showString ""
        False -> showString " nocode")
  . showPrefix oprefix True
  . showReplacementPrefix oreplprefix
  . if null derive then id else showString $
      " deriving ("
      ++ concat (intersperse ", " (map identToString derive))
      ++ ") "
showCHSHook (CHSEnumDefine ide trans derive _) =
    showString "enum define "
  . showCHSIdent ide
  . showCHSTrans trans
  . if null derive then id else showString $
      " deriving ("
      ++ concat (intersperse ", " (map identToString derive))
      ++ ") "
showCHSHook (CHSCall isPure isUns ide oalias _) =
    showString "call "
  . (if isPure then showString "pure " else id)
  . (if isUns then showString "unsafe " else id)
  . showApAlias ide oalias
showCHSHook (CHSFun isPure isUns isVar varTypes ide oalias octxt parms parm _) =
    showString "fun "
  . (if isPure then showString "pure " else id)
  . (if isUns then showString "unsafe " else id)
  . (if isVar then showString "variadic " else id)
  . showFunAlias ide varTypes oalias
  . (case octxt of
       Nothing      -> showChar ' '
       Just ctxtStr -> showString ctxtStr . showString " => ")
  . showString "{"
  . foldr (.) id (intersperse (showString ", ") (map showCHSParm parms))
  . showString "} -> "
  . showCHSParm parm
showCHSHook (CHSField acc path _) =
    (case acc of
       CHSGet -> showString "get "
       CHSSet -> showString "set ")
  . showCHSAPath path
showCHSHook (CHSOffsetof path _) =
    showString "offsetof "
  . showCHSAPath path
showCHSHook (CHSPointer star ide oalias ptrType isNewtype oRefType emit _) =
    showString "pointer "
  . (if star then showString "*" else showString "")
  . showIdAlias ide oalias
  . (case ptrType of
       CHSForeignPtr Nothing    -> showString " foreign"
       CHSForeignPtr (Just (fide, foalias)) ->
         showString " foreign finalizer " . showIdAlias fide foalias
       CHSStablePtr             -> showString " stable"
       _                        -> showString "")
  . (case (isNewtype, oRefType) of
       (True , _        ) -> showString " newtype"
       (False, []       ) -> showString ""
       (False, ides) -> showString " -> " .
                        foldr (.) id (intersperse (showString " ")
                                      (map showCHSIdent ides)))
  . (case emit of
       True  -> showString ""
       False -> showString " nocode")
showCHSHook (CHSClass oclassIde classIde typeIde _) =
    showString "class "
  . (case oclassIde of
       Nothing        -> showString ""
       Just classIde' -> showCHSIdent classIde' . showString " => ")
  . showCHSIdent classIde
  . showString " "
  . showCHSIdent typeIde
showCHSHook (CHSConst constIde _) =
    showString "const "
  . showCHSIdent constIde
showCHSHook (CHSTypedef cIde hsIde _) =
    showString "typedef "
  . showCHSIdent cIde
  . showCHSIdent hsIde
showCHSHook (CHSDefault dir hsTy cTy cPtr marsh _) =
    showString "default "
    . showString (if dir == In then "in" else "out")
    . showChar '`' . showString hsTy . showChar '\''
    . showChar '[' . showString cTy
    . showString (if cPtr then " *" else "") . showChar ']'
    . showMarsh marsh
  where showMarsh (Left ide, arg) = showCHSIdent ide . showArg arg
        showMarsh (Right s, arg) = showString s . showArg arg
        showArg CHSIOArg = showString "*"
        showArg _ = showString ""

showPrefix                        :: Maybe String -> Bool -> ShowS
showPrefix Nothing       _         = showString ""
showPrefix (Just prefix) withWith  =   maybeWith
                                     . showString "prefix = "
                                     . showString prefix
                                     . showString " "
  where
    maybeWith = if withWith then showString "with " else id

showReplacementPrefix              :: Maybe String -> ShowS
showReplacementPrefix Nothing       = showString ""
showReplacementPrefix (Just prefix) = showString "add prefix = "
                                      . showString prefix
                                      . showString " "

showIdAlias            :: Ident -> Maybe Ident -> ShowS
showIdAlias ide oalias  =
    showCHSIdent ide
  . (case oalias of
       Nothing  -> id
       Just ide' -> showString " as " . showCHSIdent ide')

showApAlias            :: CHSAPath -> Maybe Ident -> ShowS
showApAlias apath oalias  =
    showCHSAPath apath
  . (case oalias of
       Nothing  -> id
       Just ide -> showString " as " . showCHSIdent ide)

showFunAlias            :: CHSAPath -> [String] -> Maybe Ident -> ShowS
showFunAlias apath vas oalias  =
    showCHSAPath apath
  . (if null vas
     then showString ""
     else showString "["
          . foldr (.) id (intersperse (showString ", ") (map showString vas))
          . showString "]")
  . (case oalias of
       Nothing  -> id
       Just ide -> showString " as " . showCHSIdent ide)

showCHSParm                                                :: CHSParm -> ShowS
showCHSParm CHSPlusParm = showChar '+'
showCHSParm (CHSParm oimMarsh hsTyStr twoCVals oomMarsh wrapped _ comment)  =
    showOMarsh oimMarsh
  . showChar ' '
  . (if wrapped then showChar '%' else id)
  . showHsVerb hsTyStr
  . (if twoCVals then showChar '&' else id)
  . showChar ' '
  . showOMarsh oomMarsh
  . showChar ' '
  . showComment comment
  where
    showOMarsh Nothing               = id
    showOMarsh (Just (body, argKind)) =   showMarshBody body
                                        . (case argKind of
                                             CHSValArg    -> id
                                             CHSIOArg     -> showString "*"
                                             CHSVoidArg   -> showString "-"
                                             CHSIOVoidArg -> showString "*-")
    --
    showMarshBody (Left ide) = showCHSIdent ide
    showMarshBody (Right str) = showChar '|' . showString str . showChar '|'
    --
    showHsVerb str = showChar '`' . showString str . showChar '\''
    showComment str = if null str
                      then showString ""
                      else showString "--" . showString str . showChar '\n'

showCHSTrans :: CHSTrans -> ShowS
showCHSTrans (CHSTrans _2Case chgCase assocs omit)  =
    showString " {"
  . (if _2Case then showString ("underscoreToCase" ++ maybeComma) else id)
  . showCHSChangeCase chgCase
  . foldr (.) id (intersperse (showString ", ") (map showAssoc assocs))
  . showString "}"
  . (if not (null omit)
     then showString " omit (" .
          foldr (.) id (intersperse (showString ", ") (map showCHSIdent omit)) .
          showString ")"
     else id)
  where
    maybeComma = if null assocs then "" else ", "
    --
    showAssoc (ide1, ide2) =
        showCHSIdent ide1
      . showString " as "
      . showCHSIdent ide2

showCHSChangeCase :: CHSChangeCase -> ShowS
showCHSChangeCase CHSSameCase = id
showCHSChangeCase CHSUpCase   = showString "upcaseFirstLetter"
showCHSChangeCase CHSDownCase = showString "downcaseFirstLetter"

showCHSAPath :: CHSAPath -> ShowS
showCHSAPath (CHSRoot True ide) =
    showString "struct "
  . showCHSIdent ide
showCHSAPath (CHSRoot False ide) =
  showCHSIdent ide
showCHSAPath (CHSDeref path _) =
    showString "* "
  . showCHSAPath path
showCHSAPath (CHSRef (CHSDeref path _) ide) =
    showCHSAPath path
  . showString "->"
  . showCHSIdent ide
showCHSAPath (CHSRef path ide) =
   showCHSAPath path
  . showString "."
  . showCHSIdent ide

showCHSIdent :: Ident -> ShowS
showCHSIdent ide = showString $ let s = identToString ide
                                in case ' ' `elem` s of
                                  False -> s
                                  True -> "'" ++ s ++ "'"


-- load and dump a CHI file
-- ------------------------

chisuffix :: String
chisuffix  = "chi"

versionPrefix :: String
versionPrefix  = "C->Haskell Interface Version "

-- | load a CHI file
--
-- * the file suffix is automagically appended
--
-- * any error raises a syntax exception (see below)
--
-- * the version of the .chi file is checked against the version of the current
--   executable; they must match in the major and minor version
--
loadCHI       :: FilePath -> CST s String
loadCHI fname  = do
                   -- search for .chi files
                   --
                   paths <- getSwitch chiPathSB
                   let fullnames = [path </> fname <.> chisuffix |
                                    path <- paths]
                   fullname <- findFirst fullnames
                     (fatal $ fname <.> chisuffix ++ " not found in:\n"++
                              unlines paths)
                   -- read file
                   --
                   traceInfoRead fullname
                   contents <- CIO.readFile fullname

                   -- parse
                   --
                   traceInfoVersion
                   let ls = lines contents
                   when (null ls) $
                     errorCHICorrupt fname
                   let versline:chi = ls
                       prefixLen    = length versionPrefix
                   when (length versline < prefixLen
                         || take prefixLen versline /= versionPrefix) $
                     errorCHICorrupt fname
                   let versline' = drop prefixLen versline
                   (major, minor) <- case majorMinor versline' of
                                       Nothing     -> errorCHICorrupt fname
                                       Just majMin -> return majMin

                   let Just (myMajor, myMinor) = majorMinor version
                   when (major /= myMajor || minor /= myMinor) $
                     errorCHIVersion fname
                       (major ++ "." ++ minor) (myMajor ++ "." ++ myMinor)

                   -- finalize
                   --
                   traceInfoOK
                   return $ concat chi
                  where
                    traceInfoRead fname' = putTraceStr tracePhasesSW
                                             ("Attempting to read file `"
                                              ++ fname' ++ "'...\n")
                    traceInfoVersion     = putTraceStr tracePhasesSW
                                             ("...checking version `"
                                              ++ fname ++ "'...\n")
                    traceInfoOK          = putTraceStr tracePhasesSW
                                             ("...successfully loaded `"
                                              ++ fname ++ "'.\n")
                    findFirst []        err =  err
                    findFirst (p:aths)  err =  do
                      e <- CIO.doesFileExist p
                      if e then return p else findFirst aths err


-- | given a file name (no suffix) and a CHI file, the information is printed
-- into that file
--
-- * the correct suffix will automagically be appended
--
dumpCHI                :: String -> String -> CST s ()
dumpCHI fname contents  =
  do
    CIO.writeFile (fname <.> chisuffix) $
      versionPrefix ++ version ++ "\n" ++ contents

-- | extract major and minor number from a version string
--
majorMinor      :: String -> Maybe (String, String)
majorMinor vers  = let (major, rest) = break (== '.') vers
                       (minor, _   ) = break (== '.') . tail $ rest
                   in
                   if null rest then Nothing else Just (major, minor)


-- parsing a CHS token stream
-- --------------------------

syntaxExc :: String
syntaxExc  = "syntax"

-- | alternative action in case of a syntax exception
--
ifError                :: CST s a -> CST s a -> CST s a
ifError action handler  = action `catchExc` (syntaxExc, const handler)

-- | raise syntax error exception
--
raiseSyntaxError :: CST s a
raiseSyntaxError  = throwExc syntaxExc "syntax error"

-- | parse a complete module
--
-- * errors are entered into the compiler state
--
parseCHSModule        :: Position -> String -> CST s CHSModule
parseCHSModule pos cs  = do
                           toks <- lexCHS cs pos
                           frags <- parseFrags toks
                           return $ CHSModule frags

-- | parsing of code fragments
--
-- * in case of an error, all tokens that are neither Haskell nor control
--   tokens are skipped; afterwards parsing continues
--
-- * when encountering inline-C code we scan forward over all inline-C and
--   control tokens to avoid turning the control tokens within a sequence of
--   inline-C into Haskell fragments
--
parseFrags      :: [CHSToken] -> CST s [CHSFrag]
parseFrags tokens  = do
                       parseFrags0 tokens
                       `ifError` contFrags tokens
  where
    parseFrags0 :: [CHSToken] -> CST s [CHSFrag]
    parseFrags0 []                         = return []
    parseFrags0 (CHSTokHaskell pos s:toks) = do
                                               frags <- parseFrags toks
                                               return $ CHSVerb s pos : frags
    parseFrags0 (CHSTokCtrl    pos c:toks) = do
                                               frags <- parseFrags toks
                                               return $ CHSVerb [c] pos : frags
    parseFrags0 (CHSTokCPP     pos s nl:toks) = do
                                               frags <- parseFrags toks
                                               return $ CHSCPP s pos nl : frags
    parseFrags0 (CHSTokLine    pos  :toks) = do
                                               frags <- parseFrags toks
                                               return $ CHSLine pos : frags
    parseFrags0 (CHSTokC       pos s:toks) = parseC       pos s      toks
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokImport  pos  :toks) = parseImport  hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokContext pos  :toks) = parseContext hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokNonGNU  pos  :toks) = parseNonGNU  hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokType    pos  :toks) = parseType    hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokSizeof  pos  :toks) = parseSizeof  hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokAlignof pos  :toks) = parseAlignof hkpos pos
                                             (removeCommentInHook toks)
    -- TODO: issue 70, add haddock support for enum hook
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokEnum    pos  :toks) = parseEnum    hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokCall    pos  :toks) = parseCall    hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokFun     pos  :toks) = parseFun     hkpos pos toks
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokGet     pos  :toks) = parseField   hkpos pos CHSGet
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokSet     pos  :toks) = parseField   hkpos pos CHSSet
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokOffsetof pos :toks) = parseOffsetof hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokClass   pos  :toks) = parseClass   hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokConst   pos  :toks) = parseConst   hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokTypedef pos  :toks) = parseTypedef hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokDefault pos  :toks) = parseDefault hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook hkpos:
                 CHSTokPointer pos  :toks) = parsePointer hkpos pos
                                             (removeCommentInHook toks)
    parseFrags0 (CHSTokHook _       :toks) = syntaxError toks
    parseFrags0 toks                       = syntaxError toks
    --
    -- skip to next Haskell or control token
    --
    contFrags      []                       = return []
    contFrags toks@(CHSTokHaskell _ _:_   ) = parseFrags toks
    contFrags toks@(CHSTokCtrl    _ _:_   ) = parseFrags toks
    contFrags      (_                :toks) = contFrags  toks
    --
    -- Only keep comment in fun hook
    --
    isComment (CHSTokComment _ _) = True
    isComment _                   = False
    isEndHook (CHSTokEndHook _) = True
    isEndHook _                 = False
    removeCommentInHook xs = let (lhs,rhs) = span (not . isEndHook) xs
                             in filter (not . isComment) lhs ++ rhs

parseC :: Position -> String -> [CHSToken] -> CST s [CHSFrag]
parseC pos s toks =
  do
    frags <- collectCtrlAndC toks
    return $ CHSC s pos : frags
  where
    collectCtrlAndC (CHSTokCtrl pos' c :toks') = do
                                                frags <- collectCtrlAndC toks'
                                                return $ CHSC [c] pos' : frags
    collectCtrlAndC (CHSTokC    pos' s':toks') = do
                                                frags <- collectCtrlAndC toks'
                                                return $ CHSC s'  pos' : frags
    collectCtrlAndC toks'                      = parseFrags toks'

parseImport :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseImport hkpos pos toks = do
  (qual, modid, toks') <-
    case toks of
      CHSTokIdent _ ide                :toks' ->
        let (ide', toks'') = rebuildModuleId ide toks'
         in return (False, ide', toks'')
      CHSTokQualif _: CHSTokIdent _ ide:toks' ->
        let (ide', toks'') = rebuildModuleId ide toks'
         in return (True , ide', toks'')
      _                                      -> syntaxError toks
  chi <- loadCHI . moduleNameToFileName . identToString $ modid
  toks'2 <- parseEndHook toks'
  frags <- parseFrags toks'2
  return $ CHSHook (CHSImport qual modid chi pos) hkpos : frags

-- | Qualified module names do not get lexed as a single token so we need to
-- reconstruct it from a sequence of identifer and dot tokens.
--
rebuildModuleId :: Ident -> [CHSToken] -> (Ident, [CHSToken])
rebuildModuleId ide (CHSTokDot _ : CHSTokIdent _ ide' : toks) =
  let catIdent ide'3 ide'4 = internalIdentAt (posOf ide'3)
                                         --FIXME: unpleasant hack
                            (identToString ide'3 ++ '.' : identToString ide'4)
   in rebuildModuleId (catIdent ide ide') toks
rebuildModuleId ide                                     toks  = (ide, toks)

moduleNameToFileName :: String -> FilePath
moduleNameToFileName = map dotToSlash
  where dotToSlash '.' = '/'
        dotToSlash c   = c

parseContext          :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseContext hkpos pos toks  = do
  (olib    , toks2) <- parseOptLib          toks
  (opref   , toks3) <- parseOptPrefix False toks2
  (oreppref, toks4) <- parseOptReplPrefix   toks3
  toks5             <- parseEndHook         toks4
  frags             <- parseFrags           toks5
  let frag = CHSContext olib opref oreppref pos
  return $ CHSHook frag hkpos : frags

parseNonGNU           :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseNonGNU hkpos pos toks  = do
  toks2             <- parseEndHook         toks
  frags             <- parseFrags           toks2
  let frag = CHSNonGNU pos
  return $ CHSHook frag hkpos : frags

parseType :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseType hkpos pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSType ide pos) hkpos : frags
parseType _ _ toks = syntaxError toks

parseSizeof :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseSizeof hkpos pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSSizeof ide pos) hkpos : frags
parseSizeof _ _ toks = syntaxError toks

parseAlignof :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseAlignof hkpos pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSAlignof ide pos) hkpos : frags
parseAlignof _ _ toks = syntaxError toks

parseEnum :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]

-- {#enum define hsid {alias_1,...,alias_n}  [deriving (clid_1,...,clid_n)] #}
parseEnum hkpos pos (CHSTokIdent _ def: CHSTokIdent _ hsid: toks)
  | identToString def == "define" =
  do
    (trans , toks')   <- parseTrans          toks
    (derive, toks'')  <- parseDerive         toks'
    toks'''           <- parseEndHook        toks''
    frags             <- parseFrags          toks'''
    return $ CHSHook (CHSEnumDefine hsid trans derive pos) hkpos : frags

-- {#enum cid [as hsid] {alias_1,...,alias_n}  [with prefix = pref] [deriving (clid_1,...,clid_n)] #}
parseEnum hkpos pos (CHSTokIdent _ ide:toks) =
  do
    (oalias,      toks2) <- parseOptAs ide True toks
    (emit,        toks3) <- parseOptNoCode      toks2
    (trans,       toks4) <- parseTrans          toks3
    (oprefix,     toks5) <- parseOptPrefix True toks4
    (oreplprefix, toks6) <- parseOptReplPrefix  toks5
    (derive,      toks7) <- parseDerive         toks6
    toks8                <- parseEndHook        toks7
    frags                <- parseFrags          toks8
    return $ CHSHook (CHSEnum ide (norm oalias) trans emit
                      oprefix oreplprefix derive pos) hkpos : frags
  where
    norm Nothing                   = Nothing
    norm (Just ide') | ide == ide' = Nothing
                     | otherwise   = Just ide'
parseEnum _ _ toks = syntaxError toks

parseOptNoCode :: [CHSToken] -> CST s (Bool, [CHSToken])
parseOptNoCode (CHSTokNocode _ :toks) = return (False, toks)
parseOptNoCode toks                   = return (True, toks)

parseCall          :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseCall hkpos pos toks  =
  do
    (isPure  , toks'   ) <- parseIsPure          toks
    (isUnsafe, toks''  ) <- parseIsUnsafe        toks'
    (apath   , toks''' ) <- parsePath            toks''
    (oalias  , toks'''') <- parseOptAs (apathToIdent apath) False toks'''
    toks'''''            <- parseEndHook         toks''''
    frags                <- parseFrags           toks'''''
    return $
      CHSHook (CHSCall isPure isUnsafe apath oalias pos) hkpos : frags

parseFun          :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseFun hkpos pos inputToks  =
  do
    (isPure  , toks' ) <- parseIsPure          toks
    (isUnsafe, toks'2) <- parseIsUnsafe        toks'
    (isVar,    toks'3) <- parseIsVariadic      toks'2
    (apath   , toks'4) <- parsePath            toks'3
    (varTypes, toks'5) <- parseVarTypes        toks'4
    (oalias  , toks'6) <- parseOptAs (apathToIdent apath) False toks'5
    (octxt   , toks'7) <- parseOptContext      toks'6
    (parms   , toks'8) <- parseParms           toks'7
    (parm    , toks'9) <- parseParm            toks'8
    when (isParmWrapped parm) $ errorOutWrap $ head toks'8
    toks'10            <- parseEndHook         toks'9
    frags              <- parseFrags           toks'10
    return $
      CHSHook
        (CHSFun isPure isUnsafe isVar varTypes
         apath oalias octxt parms parm pos) hkpos :
      frags
  where
    toks = removeIllPositionedComment inputToks
    parseOptContext (CHSTokHSVerb _ ctxt:CHSTokDArrow _:toks') =
      return (Just ctxt, toks')
    parseOptContext toks'                                      =
      return (Nothing  , toks')
    --
    parseVarTypes (CHSTokLBrack _:CHSTokCArg _ t:toks') = do
      (ts, toks'2) <- parseVarTypes' toks'
      return (t:ts, toks'2)
    parseVarTypes toks' = return ([], toks')
    parseVarTypes' (CHSTokRBrack _:toks') = return ([], toks')
    parseVarTypes' (CHSTokComma _:CHSTokCArg _ t:toks') = do
      (ts, toks'2) <- parseVarTypes' toks'
      return (t:ts, toks'2)
    --
    parseParms (CHSTokLBrace _:CHSTokRBrace _:CHSTokArrow _:toks') =
      return ([], toks')
    parseParms (CHSTokLBrace _                             :toks') =
      parseParms' (CHSTokComma nopos:toks')
    parseParms                                              toks'  =
      syntaxError toks'
    --
    parseParms' (CHSTokRBrace _:CHSTokArrow _:toks') = return ([], toks')
    parseParms' (CHSTokComma _:CHSTokComment _ _:toks') = do
      (parm , toks'2 ) <- parseParm   toks'
      (parms, toks'3)  <- parseParms' toks'2
      return (parm:parms, toks'3)
    parseParms' (CHSTokComma _               :toks') = do
      (parm , toks'2 ) <- parseParm   toks'
      (parms, toks'3)  <- parseParms' toks'2
      return (parm:parms, toks'3)
    parseParms' (CHSTokRBrace _              :toks') = syntaxError toks'
      -- gives better error messages
    parseParms'                               toks'  = syntaxError toks'
    --
    isComment (CHSTokComment _ _) = True
    isComment _ = False
    isLBrace (CHSTokLBrace _) = True
    isLBrace _ = False
    isRBrace (CHSTokRBrace _) = True
    isRBrace _ = False
    isHSVerb (CHSTokHSVerb _ _) = True
    isHSVerb _ = False
    -- remove comment(s) between
    -- 1. {# and {
    -- 2. } and `ResultType'
    removeIllPositionedComment xs = let (lhs,rhs) = span (not . isLBrace) xs
                                        (lhs',rhs') = span (not . isRBrace) rhs
                                        (lhs'2,rhs'2) = span (not . isHSVerb) rhs'
                                    in filter (not . isComment) lhs ++ lhs' ++
                                       (filter (not . isComment) lhs'2) ++ rhs'2


parseIsPure :: [CHSToken] -> CST s (Bool, [CHSToken])
parseIsPure (CHSTokPure _:toks) = return (True , toks)
parseIsPure (CHSTokFun  _:toks) = return (True , toks)  -- backwards compat.
parseIsPure toks                = return (False, toks)
-- FIXME: eventually, remove `fun'; it's currently deprecated

parseIsUnsafe :: [CHSToken] -> CST s (Bool, [CHSToken])
parseIsUnsafe (CHSTokUnsafe _:toks) = return (True , toks)
parseIsUnsafe toks                  = return (False, toks)

parseIsVariadic :: [CHSToken] -> CST s (Bool, [CHSToken])
parseIsVariadic (CHSTokVariadic _:toks) = return (True , toks)
parseIsVariadic toks                    = return (False, toks)

apathToIdent :: CHSAPath -> Ident
apathToIdent (CHSRoot _ ide) =
    let lowerFirst (c:cs) = toLower c : cs
    in internalIdentAt (posOf ide) (lowerFirst $ identToString ide)
apathToIdent (CHSDeref apath _) =
    let ide = apathToIdent apath
    in internalIdentAt (posOf ide) (identToString ide ++ "_")
apathToIdent (CHSRef apath ide') =
    let ide = apathToIdent apath
        upperFirst (c:cs) = toLower c : cs
        sel = upperFirst $ identToString ide'
    in internalIdentAt  (posOf ide) (identToString ide ++ sel)

apathRootIdent :: CHSAPath -> Ident
apathRootIdent (CHSRoot _ ide) = ide
apathRootIdent (CHSDeref apath _) = apathRootIdent apath
apathRootIdent (CHSRef apath _) = apathRootIdent apath

parseParm :: [CHSToken] -> CST s (CHSParm, [CHSToken])
parseParm (CHSTokPlus _:toks') = return (CHSPlusParm, toks')
parseParm toks =
  do
    (oimMarsh, toks' ) <- parseOptMarsh toks
    let (wrapped, toks'') = case toks' of
          (CHSTokPercent _:tokstmp) -> (True,  tokstmp)
          _                         -> (False, toks')
    (hsTyStr, twoCVals, pos, toks'2) <-
      case toks'' of
        (CHSTokHSVerb pos hsTyStr:CHSTokAmp _:toks'2) ->
          return (hsTyStr, True , pos, toks'2)
        (CHSTokHSVerb pos hsTyStr            :toks'2) ->
          return (hsTyStr, False, pos, toks'2)
        _toks                                          -> syntaxError toks''
    (oomMarsh, toks'3) <- parseOptMarsh toks'2
    (comments, toks'4) <- parseOptComments toks'3
    return (CHSParm oimMarsh hsTyStr twoCVals oomMarsh wrapped pos
            (concat (intersperse " " comments)), toks'4)
  where
    parseOptMarsh :: [CHSToken] -> CST s (CHSMarsh, [CHSToken])
    parseOptMarsh (CHSTokIdent _ ide:toks') =
      do
        (marshType, toks'2) <- parseOptMarshType toks'
        return (Just (Left ide, marshType), toks'2)
    parseOptMarsh (CHSTokHSQuot _ str:toks') =
      do
        (marshType, toks'2) <- parseOptMarshType toks'
        return (Just (Right str, marshType), toks'2)
    parseOptMarsh (CHSTokWith _ ide:toks') =
      do
        (marshType, toks'2) <- parseOptMarshType toks'
        return (Just (Left ide, marshType), toks'2)
    parseOptMarsh toks'                     =
      return (Nothing, toks')

    parseOptMarshType (CHSTokStar _ :CHSTokMinus _:toks') =
      return (CHSIOVoidArg , toks')
    parseOptMarshType (CHSTokStar _ :toks') =
      return (CHSIOArg , toks')
    parseOptMarshType (CHSTokMinus _:toks') =
      return (CHSVoidArg, toks')
    parseOptMarshType toks' =
      return (CHSValArg, toks')

parseOptComments :: [CHSToken] -> CST s ([String], [CHSToken])
parseOptComments = go []
  where
    go acc (CHSTokComment _ s:toks) = go (s:acc) toks
    go acc _toks = return (reverse acc,_toks)

parseField :: Position -> Position -> CHSAccess -> [CHSToken] -> CST s [CHSFrag]
parseField hkpos pos access toks =
  do
    (path, toks') <- parsePath  toks
    toks''        <- parseEndHook toks'
    frags         <- parseFrags toks''
    return $ CHSHook (CHSField access path pos) hkpos : frags

parseOffsetof :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseOffsetof hkpos pos toks =
  do
    (path, toks') <- parsePath toks
    toks''        <- parseEndHook toks'
    frags         <- parseFrags toks''
    return $ CHSHook (CHSOffsetof path pos) hkpos : frags

parsePointer :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parsePointer hkpos pos toks = do
    (isStar, ide, toks')          <-
      case toks of
        CHSTokStar _:CHSTokIdent _ ide:toks' -> return (True , ide, toks')
        CHSTokIdent _ ide             :toks' -> return (False, ide, toks')
        _                                    -> syntaxError toks
    (oalias , toks'2)             <- parseOptAs ide True toks'
    (ptrType, toks'3)             <- parsePtrType        toks'2
    let
     (isNewtype, oRefType, toks'4) =
      case toks'3 of
        CHSTokNewtype _                   :toks'' -> (True , [] , toks'' )
        CHSTokArrow   _:CHSTokIdent _ ide':toks'' ->
          let (ides, toks''') = span isIde toks''
              isIde (CHSTokIdent _ _) = True
              isIde _                 = False
              takeId (CHSTokIdent _ i) = i
          in (False, ide':map takeId ides, toks''')
        CHSTokArrow   _:CHSTokHSVerb _ hs:toks'' ->
          (False, map internalIdent $ words hs, toks'')
        _                                         -> (False, [] , toks'3)
    let
     (emit, toks'5) =
      case toks'4 of
        CHSTokNocode _                  :toks'' -> (False, toks'' )
        _                                       -> (True , toks'4 )
    toks'6                        <- parseEndHook toks'5
    frags                         <- parseFrags   toks'6
    return $
      CHSHook
       (CHSPointer
         isStar ide (norm ide oalias) ptrType isNewtype
         oRefType emit pos) hkpos
       : frags
  where
    parsePtrType :: [CHSToken] -> CST s (CHSPtrType, [CHSToken])
    parsePtrType (CHSTokForeign _:toks') = do
      (final, toks'') <- parseFinalizer toks'
      return (CHSForeignPtr final, toks'')
    parsePtrType (CHSTokStable _ :toks') = return (CHSStablePtr, toks')
    parsePtrType                  toks'  = return (CHSPtr, toks')

    parseFinalizer (CHSTokFinal _ : CHSTokIdent _ ide : toks') = do
      (oalias, toks'') <- parseOptAs ide False toks'
      return (Just (ide, oalias), toks'')
    parseFinalizer toks' = return (Nothing, toks')

    norm _   Nothing                   = Nothing
    norm ide (Just ide') | ide == ide' = Nothing
                         | otherwise   = Just ide'

parseClass :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseClass hkpos pos (CHSTokIdent  _ sclassIde:
                CHSTokDArrow _          :
                CHSTokIdent  _ classIde :
                CHSTokIdent  _ typeIde  :
                toks)                     =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSClass (Just sclassIde)
                      classIde typeIde pos) hkpos : frags
parseClass hkpos pos (CHSTokIdent _ classIde :
                      CHSTokIdent _ typeIde  :
                      toks)                     =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSClass Nothing classIde typeIde pos) hkpos : frags
parseClass _ _ toks = syntaxError toks

parseConst :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseConst hkpos pos (CHSTokIdent  _ constIde : toks)                     =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSConst constIde pos) hkpos : frags
parseConst _ _ toks = syntaxError toks

parseTypedef :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseTypedef hkpos pos (CHSTokIdent _ cIde : CHSTokIdent _ hsIde :
                        CHSTokEndHook _ : toks) =
  do
    frags <- parseFrags toks
    return $ CHSHook (CHSTypedef cIde hsIde pos) hkpos : frags
parseTypedef _ _ toks = syntaxError toks

parseDefault :: Position -> Position -> [CHSToken] -> CST s [CHSFrag]
parseDefault hkpos pos
  toks@(dirtok :
        CHSTokHSVerb _ hsTy :
        CHSTokLBrack _ :
        CHSTokCArg _ cTyIn :
        CHSTokRBrack _ :
        toks1) =
  do
    dir <- case dirtok of
      CHSTokIn _  -> return In
      CHSTokOut _ -> return Out
      _           -> syntaxError toks
    (marsh, toks2) <- parseMarshaller toks1
    let trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
        cTy' = trim cTyIn
        (cTy, cPtr) = if last cTy' == '*'
                      then (trim $ init cTy', True)
                      else (cTy', False)
    toks3 <- parseEndHook toks2
    frags <- parseFrags toks3
    return $ CHSHook (CHSDefault dir hsTy cTy cPtr marsh pos) hkpos : frags
  where parseMarshaller :: [CHSToken]
                         -> CST s ((Either Ident String, CHSArg), [CHSToken])
        parseMarshaller (CHSTokIdent _ mide : toks') = do
            (hasStar, toks'') <- parseOptStar toks'
            let argtype = if hasStar then CHSIOArg else CHSValArg
            return ((Left mide, argtype), toks'')
        parseMarshaller toks' = syntaxError toks'
parseDefault _ _ toks = syntaxError toks

parseOptStar :: [CHSToken] -> CST s (Bool, [CHSToken])
parseOptStar (CHSTokStar _ : toks) = return (True, toks)
parseOptStar toks = return (False, toks)

parseOptLib :: [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptLib (CHSTokLib    _    :
             CHSTokEqual  _    :
             CHSTokString _ str:
             toks)                = return (Just str, toks)
parseOptLib (CHSTokLib _:toks   ) = syntaxError toks
parseOptLib toks                  = return (Nothing, toks)

parseOptPrefix :: Bool -> [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptPrefix False (CHSTokPrefix _    :
                      CHSTokEqual  _    :
                      CHSTokString _ str:
                      toks)                = return (Just str, toks)
parseOptPrefix True  (CHSTokWith   _ _  :
                      CHSTokPrefix _    :
                      CHSTokEqual  _    :
                      CHSTokString _ str:
                      toks)                = return (Just str, toks)
parseOptPrefix _     (CHSTokWith _ _:toks) = syntaxError toks
parseOptPrefix _     (CHSTokPrefix _:toks) = syntaxError toks
parseOptPrefix _     toks                  = return (Nothing, toks)

parseOptReplPrefix :: [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptReplPrefix (CHSTokAdd   _    :
                    CHSTokPrefix _    :
                    CHSTokEqual  _    :
                    CHSTokString _ str:
                    toks)                = return (Just str, toks)
parseOptReplPrefix (CHSTokAdd    _:toks) = syntaxError toks
parseOptReplPrefix (CHSTokPrefix _:toks) = syntaxError toks
parseOptReplPrefix toks                  = return (Nothing, toks)

-- first argument is the identifier that is to be used when `^' is given and
-- the second indicates whether the first character has to be upper case
--
parseOptAs :: Ident -> Bool -> [CHSToken] -> CST s (Maybe Ident, [CHSToken])
parseOptAs _   _     (CHSTokAs _:CHSTokIdent _ ide:toks) =
  return (Just ide, toks)
parseOptAs _   _     (CHSTokAs _:CHSTokHSQuot pos ide:toks) =
  return (Just $ internalIdentAt pos ide, toks)
parseOptAs ide upper (CHSTokAs _:CHSTokHat pos    :toks) =
  return (Just $ underscoreToCase ide upper pos, toks)
parseOptAs _   _     (CHSTokAs _                  :toks) = syntaxError toks
parseOptAs _   _                                   toks  =
  return (Nothing, toks)

-- | convert C style identifier to Haskell style identifier
--
underscoreToCase               :: Ident -> Bool -> Position -> Ident
underscoreToCase ide upper pos  =
  let lexeme = identToString ide
      ps     = filter (not . null) . parts $ lexeme
  in
  internalIdentAt pos . adjustHead . concat . map adjustCase $ ps
  where
    parts s = let (l, s') = break (== '_') s
              in
              l : case s' of
                    []      -> []
                    (_:s'') -> parts s''
    --
    adjustCase ""     = ""
    adjustCase (c:cs) = toUpper c : cs
    --
    adjustHead ""     = ""
    adjustHead (c:cs) = if upper then toUpper c : cs else toLower c:cs

-- | this is disambiguated and left factored
--
parsePath :: [CHSToken] -> CST s (CHSAPath, [CHSToken])
parsePath (CHSTokLParen _pos: toks) =
  do
    (inner_path, toks_rest) <- parsePath toks
    toks_rest' <- case toks_rest of
                    (CHSTokRParen _pos' : ts) -> return ts
                    _ -> syntaxError toks_rest
    (pathWithHole, toks') <- parsePath' toks_rest'
    return (pathWithHole inner_path, toks')
parsePath (CHSTokStar pos:toks) =
  do
    (path, toks') <- parsePath toks
    return (CHSDeref path pos, toks')
parsePath (CHSTokStruct _pos:tok:toks) =
  case keywordToIdent tok of
    (CHSTokIdent _ ide) ->
      do
        (pathWithHole, toks') <- parsePath' toks
        return (pathWithHole (CHSRoot True ide), toks')
    _ -> syntaxError (tok:toks)
parsePath (tok:toks) =
  case keywordToIdent tok of
    (CHSTokIdent _ ide) ->
      do
        (pathWithHole, toks') <- parsePath' toks
        return (pathWithHole (CHSRoot False ide), toks')
    _ -> syntaxError (tok:toks)
parsePath toks = syntaxError toks

-- | @s->m@ is represented by @(*s).m@ in the tree
--
parsePath' :: [CHSToken] -> CST s (CHSAPath -> CHSAPath, [CHSToken])
parsePath' tokens@(CHSTokDot _:desig:toks) =
  do
    ide <- case keywordToIdent desig of CHSTokIdent _ i -> return i; _ -> syntaxError tokens
    (pathWithHole, toks') <- parsePath' toks
    return (pathWithHole . (\hole -> CHSRef hole ide), toks')
parsePath' tokens@(CHSTokArrow pos:desig:toks) =
  do
    ide <- case keywordToIdent desig of CHSTokIdent _ i -> return i; _ -> syntaxError tokens
    (pathWithHole, toks') <- parsePath' toks
    return (pathWithHole . (\hole -> CHSRef (CHSDeref hole pos) ide), toks')
parsePath' toks =
    return (id,toks)

parseTrans :: [CHSToken] -> CST s (CHSTrans, [CHSToken])
parseTrans (CHSTokLBrace _:toks) =
  do
    (_2Case, chgCase, toks' ) <- parse_2CaseAndChange toks
    case toks' of
      (CHSTokRBrace _:toks'2) -> do
        (omits, toks'3) <- parseOmits toks'2
        return (CHSTrans _2Case chgCase [] omits, toks'3)
      _                       ->
        do
          -- if there was no `underscoreToCase', we add a comma token to meet
          -- the invariant of `parseTranss'
          --
          (transs, toks'2) <- if (_2Case || chgCase /= CHSSameCase)
                              then parseTranss toks'
                              else parseTranss (CHSTokComma nopos:toks')
          (omits, toks'3) <- parseOmits toks'2
          return (CHSTrans _2Case chgCase transs omits, toks'3)
  where
    parse_2CaseAndChange (CHSTok_2Case _:CHSTokComma _:CHSTokUpper _:toks') =
      return (True, CHSUpCase, toks')
    parse_2CaseAndChange (CHSTok_2Case _:CHSTokComma _:CHSTokDown _ :toks') =
      return (True, CHSDownCase, toks')
    parse_2CaseAndChange (CHSTok_2Case _                            :toks') =
      return (True, CHSSameCase, toks')
    parse_2CaseAndChange (CHSTokUpper _:CHSTokComma _:CHSTok_2Case _:toks') =
      return (True, CHSUpCase, toks')
    parse_2CaseAndChange (CHSTokUpper _                             :toks') =
      return (False, CHSUpCase, toks')
    parse_2CaseAndChange (CHSTokDown  _:CHSTokComma _:CHSTok_2Case _:toks') =
      return (True, CHSDownCase, toks')
    parse_2CaseAndChange (CHSTokDown  _                             :toks') =
      return (False, CHSDownCase, toks')
    parse_2CaseAndChange toks'                                              =
      return (False, CHSSameCase, toks')
    --
    parseTranss (CHSTokRBrace _:toks') = return ([], toks')
    parseTranss (CHSTokComma  _:toks') = do
                                        (assoc, toks'2 ) <- parseAssoc toks'
                                        (trans, toks'3) <- parseTranss toks'2
                                        return (assoc:trans, toks'3)
    parseTranss toks'                  = syntaxError toks'
    --
    parseOmits (CHSTokOmit _:CHSTokLParen _:CHSTokIdent _ omit:toks') = do
      (omits, toks'2) <- parseOmits1 toks'
      return (omit:omits, toks'2)
    parseOmits toks' = return ([], toks')
    --
    parseOmits1 (CHSTokRParen _:toks') = return ([], toks')
    parseOmits1 (CHSTokComma _:CHSTokIdent _ omit:toks') = do
      (omits, toks'2) <- parseOmits1 toks'
      return (omit:omits, toks'2)
    parseOmits1 toks' = syntaxError toks'
    --
    parseAssoc (CHSTokIdent _ ide1:CHSTokAs _:CHSTokIdent _ ide2:toks') =
      return ((ide1, ide2), toks')
    parseAssoc (CHSTokCIdentTail _ ide1:CHSTokAs _:CHSTokIdent _ ide2:toks') =
      return ((ide1, ide2), toks')
    parseAssoc (CHSTokIdent _ _   :CHSTokAs _:toks'                   ) =
      syntaxError toks'
    parseAssoc (CHSTokIdent _ _   :toks'                              ) =
      syntaxError toks'
    parseAssoc toks'                                                    =
      syntaxError toks'
parseTrans toks = syntaxError toks

parseDerive :: [CHSToken] -> CST s ([Ident], [CHSToken])
parseDerive (CHSTokDerive _ :CHSTokLParen _:CHSTokRParen _:toks) =
  return ([], toks)
parseDerive (CHSTokDerive _ :CHSTokLParen _:toks)                =
  parseCommaIdent (CHSTokComma nopos:toks)
  where
    parseCommaIdent :: [CHSToken] -> CST s ([Ident], [CHSToken])
    parseCommaIdent (CHSTokComma _:CHSTokIdent _ ide:toks') =
      do
        (ids, tok') <- parseCommaIdent toks'
        return (ide:ids, tok')
    parseCommaIdent (CHSTokRParen _                :toks') =
      return ([], toks')
parseDerive toks = return ([],toks)

parseEndHook :: [CHSToken] -> CST s ([CHSToken])
parseEndHook (CHSTokEndHook _:toks) = return toks
parseEndHook toks                   = syntaxError toks

syntaxError         :: [CHSToken] -> CST s a
syntaxError []       = errorEOF
syntaxError (tok:_)  = errorIllegal tok

errorIllegal     :: CHSToken -> CST s a
errorIllegal tok  = do
                      raiseError (posOf tok)
                        ["Syntax error!",
                         "The phrase `" ++ show tok ++ "' is not allowed \
                         \here."]
                      raiseSyntaxError

errorEOF :: CST s a
errorEOF  = do
              raiseError nopos
                ["Premature end of file!",
                 "The .chs file ends in the middle of a binding hook."]
              raiseSyntaxError

errorOutWrap :: CHSToken -> CST s a
errorOutWrap tok = do
  raiseError (posOf tok)
    ["Syntax error!",
     "Structure wrapping is not allowed for return parameters."]
  raiseSyntaxError

errorCHICorrupt      :: String -> CST s a
errorCHICorrupt ide  = do
  raiseError nopos
    ["Corrupt .chi file!",
     "The file `" ++  ide ++ ".chi' is corrupt."]
  raiseSyntaxError

errorCHIVersion :: String -> String -> String -> CST s a
errorCHIVersion ide chiVersion myVersion  = do
  raiseError nopos
    ["Wrong version of .chi file!",
     "The file `" ++ ide ++ ".chi' is version "
     ++ chiVersion ++ ", but mine is " ++ myVersion ++ "."]
  raiseSyntaxError
