--  C->Haskell Compiler: CHS file abstraction
--
--  Author : Manuel M T Chakravarty
--  Created: 16 August 99
--
--  Version $Revision: 1.21 $ from $Date: 2002/03/20 03:50:07 $
--
--  Copyright (c) [1999..2002] Manuel M T Chakravarty
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
--	      | `context' ctxt
--	      | `type' ident
--	      | `sizeof' ident
--	      | `enum' idalias trans [`with' prefix] [deriving]
--	      | `call' [`pure'] [`unsafe'] idalias
--	      | `fun' [`pure'] [`unsafe'] idalias parms
--	      | `get' apath
--	      | `set' apath
--	      | `pointer' ['*'] idalias ptrkind
--	      | `class' [ident `=>'] ident ident
--  ctxt     -> [`header' `=' string] [`lib' `=' string] [prefix]
--  idalias  -> ident [`as' (ident | `^')]
--  prefix   -> `prefix' `=' string
--  deriving -> `deriving' `(' ident_1 `,' ... `,' ident_n `)'
--  parms    -> [verbhs `=>'] `{' parm_1 `,' ... `,' parm_n `}' `->' parm
--  parm     -> [ident_1 [`*' | `-']] verbhs [`&'] [ident_2 [`*' | `-']]
--  apath    -> ident
--	      | `*' apath
--	      | apath `.' ident
--	      | apath `->' ident
--  trans    -> `{' alias_1 `,' ... `,' alias_n `}'
--  alias    -> `underscoreToCase'
--	      | ident `as' ident
--  ptrkind  -> [`foreign' | `stable'] ['newtype' | '->' ident]
--  
--  If `underscoreToCase' occurs in a translation table, it must be the first
--  entry.
--
--  Remark: Optional Haskell names are normalised during structure tree
--	    construction, ie, associations that associated a name with itself
--	    are removed.  (They don't carry semantic content, and make some
--	    tests more complicated.)
--
--- TODO ----------------------------------------------------------------------
--

module CHS (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..), CHSParm(..),
	    CHSArg(..), CHSAccess(..), CHSAPath(..), CHSPtrType(..),
	    loadCHS, dumpCHS, hssuffix, chssuffix, loadCHI, dumpCHI,
	    chisuffix, showCHSParm)
where 

-- standard libraries
import Char	 (isSpace, toUpper, toLower)
import List	 (intersperse)
import Monad	 (when)

-- Compiler Toolkit
import Common    (Position, Pos(posOf), nopos)
import Errors	 (interr)
import Idents    (Ident, identToLexeme, onlyPosIdent)

-- C->Haskell
import C2HSState (CST, doesFileExistCIO, readFileCIO, writeFileCIO, getId, 
		  getSwitch, chiPathSB, catchExc, throwExc, raiseError, 
		  fatal, errorsPresent, showErrors, Traces(..), putTraceStr) 

-- friends
import CHSLexer  (CHSToken(..), lexCHS)


-- CHS abstract syntax
-- -------------------

-- representation of a CHS module (EXPORTED)
--
data CHSModule = CHSModule [CHSFrag]

-- a CHS code fragament (EXPORTED)
--
data CHSFrag = CHSVerb String
	     | CHSHook CHSHook

-- a CHS binding hook (EXPORTED)
--
data CHSHook = CHSImport  Bool			-- qualified?
			  Ident			-- module name
			  String		-- content of .chi file
			  Position
	     | CHSContext (Maybe String)	-- header name
			  (Maybe String)	-- library name
			  (Maybe String)	-- prefix
			  Position
	     | CHSType    Ident			-- C type
			  Position
	     | CHSSizeof  Ident			-- C type
			  Position
	     | CHSEnum    Ident			-- C enumeration type
			  (Maybe Ident)		-- Haskell name
			  CHSTrans		-- translation table
			  (Maybe String)	-- local prefix
			  [Ident]		-- instance requests from user
			  Position
	     | CHSCall    Bool			-- is a pure function?
			  Bool			-- is unsafe?
			  Ident			-- C function
			  (Maybe Ident)		-- Haskell name
			  Position
	     | CHSFun     Bool			-- is a pure function?
			  Bool			-- is unsafe?
			  Ident			-- C function
			  (Maybe Ident)		-- Haskell name
			  (Maybe String)	-- type context
			  [CHSParm]		-- argument marshalling
			  CHSParm		-- result marshalling
			  Position
	     | CHSField   CHSAccess		-- access type
			  CHSAPath		-- access path
			  Position 
	     | CHSPointer Bool			-- explicit '*' in hook
			  Ident			-- C pointer name
			  (Maybe Ident)		-- Haskell name
			  CHSPtrType		-- Ptr, ForeignPtr or StablePtr
			  Bool			-- create new type?
			  (Maybe Ident)		-- Haskell type pointed to
			  Position
             | CHSClass   (Maybe Ident)		-- superclass
			  Ident			-- class name
			  Ident			-- name of pointer type
			  Position

instance Pos CHSHook where
  posOf (CHSImport  _ _ _         pos) = pos
  posOf (CHSContext _ _ _         pos) = pos
  posOf (CHSType    _             pos) = pos
  posOf (CHSSizeof  _             pos) = pos
  posOf (CHSEnum    _ _ _ _ _     pos) = pos
  posOf (CHSCall    _ _ _ _       pos) = pos
  posOf (CHSFun     _ _ _ _ _ _ _ pos) = pos
  posOf (CHSField   _ _           pos) = pos
  posOf (CHSPointer _ _ _ _ _ _   pos) = pos
  posOf (CHSClass   _ _ _	  pos) = pos

-- two hooks are equal if they have the same Haskell name and reference the
-- same C object 
--
instance Eq CHSHook where
  (CHSImport qual1 ide1 _      _) == (CHSImport qual2 ide2 _      _) =    
    qual1 == qual2 && ide1 == ide2
  (CHSContext h1 olib1 opref1  _) == (CHSContext h2 olib2 opref2  _) =    
    h1 == h2 && olib1 == olib1 && opref1 == opref2
  (CHSType ide1                _) == (CHSType ide2                _) = 
    ide1 == ide2
  (CHSSizeof ide1              _) == (CHSSizeof ide2              _) = 
    ide1 == ide2
  (CHSEnum ide1 oalias1 _ _ _  _) == (CHSEnum ide2 oalias2 _ _ _  _) = 
    oalias1 == oalias2 && ide1 == ide2
  (CHSCall _ _ ide1 oalias1    _) == (CHSCall _ _ ide2 oalias2    _) = 
    oalias1 == oalias2 && ide1 == ide2
  (CHSFun  _ _ ide1 oalias1 _ _ _ _) 
			          == (CHSFun _ _ ide2 oalias2 _ _ _ _) = 
    oalias1 == oalias2 && ide1 == ide2
  (CHSField acc1 path1         _) == (CHSField acc2 path2         _) =    
    acc1 == acc2 && path1 == path2
  (CHSPointer _ ide1 oalias1 _ _ _ _) 
			          == (CHSPointer _ ide2 oalias2 _ _ _ _) =
    ide1 == ide2 && oalias1 == oalias2
  (CHSClass _ ide1 _           _) == (CHSClass _ ide2 _           _) =
    ide1 == ide2
  _                               == _                          = False

-- translation table (EXPORTED)
--
data CHSTrans = CHSTrans Bool			-- underscore to case?
			 [(Ident, Ident)]	-- alias list

-- marshalling descriptor for function hooks (EXPORTED)
--
-- * a marshaller consists of a function name and flag indicating whether it
--   has to be executed in the IO monad
--
data CHSParm = CHSParm (Maybe (Ident, CHSArg))	-- "in" marshaller
		       String			-- Haskell type
		       Bool			-- C repr: two values?
		       (Maybe (Ident, CHSArg))	-- "out" marshaller
		       Position

-- kinds of arguments in function hooks (EXPORTED)
--
data CHSArg = CHSValArg				-- plain value argument
	    | CHSIOArg				-- reference argument
	    | CHSVoidArg			-- no argument
	    deriving (Eq)

-- structure member access types (EXPORTED)
--
data CHSAccess = CHSSet				-- set structure field
	       | CHSGet				-- get structure field
	       deriving (Eq)

-- structure access path (EXPORTED)
--
data CHSAPath = CHSRoot  Ident			-- root of access path
	      | CHSDeref CHSAPath Position	-- dereferencing
	      | CHSRef	 CHSAPath Ident		-- member referencing
	      deriving (Eq)

-- pointer options (EXPORTED)
--

data CHSPtrType = CHSPtr			-- standard Ptr from Haskell
		| CHSForeignPtr			-- a pointer with a finalizer
		| CHSStablePtr			-- a pointer into Haskell land
		deriving (Eq)

instance Show CHSPtrType where
  show CHSPtr		 = "Ptr"
  show CHSForeignPtr	 = "ForeignPtr"
  show CHSStablePtr	 = "StablePtr"

instance Read CHSPtrType where
  readsPrec _ (                            'P':'t':'r':rest) = 
    [(CHSPtr, rest)]
  readsPrec _ ('F':'o':'r':'e':'i':'g':'n':'P':'t':'r':rest) = 
    [(CHSForeignPtr, rest)]
  readsPrec _ ('S':'t':'a':'b':'l':'e'    :'P':'t':'r':rest) = 
    [(CHSStablePtr, rest)]
  readsPrec p (c:cs)
    | isSpace c						     = readsPrec p cs
  readsPrec _ _						     = []


-- load and dump a CHS file
-- ------------------------

hssuffix, chssuffix :: String
hssuffix  = ".hs"
chssuffix = ".chs"

-- load a CHS module (EXPORTED)
--
-- * the file suffix is automagically appended
--
-- * in case of a syntactical or lexical error, a fatal error is raised;
--   warnings are returned together with the module
--
loadCHS       :: FilePath -> CST s (CHSModule, String)
loadCHS fname  = do
		   let fullname = fname ++ chssuffix

		   -- read file
		   --
		   traceInfoRead fullname
		   contents <- readFileCIO fullname

		   -- parse
		   --
		   traceInfoParse
		   mod <- parseCHSModule (fullname, 1, 1) contents

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
		       return (mod, warnmsgs)
		  where
		    traceInfoRead fname = putTraceStr tracePhasesSW
					    ("Attempting to read file `"
					     ++ fname ++ "'...\n")
		    traceInfoParse      = putTraceStr tracePhasesSW 
					    ("...parsing `" 
					     ++ fname ++ "'...\n")
		    traceInfoErr        = putTraceStr tracePhasesSW
					    ("...error(s) detected in `"
					     ++ fname ++ "'.\n")
		    traceInfoOK         = putTraceStr tracePhasesSW
					    ("...successfully loaded `"
					     ++ fname ++ "'.\n")

-- given a file name (no suffix) and a CHS module, the module is printed 
-- into that file (EXPORTED)
-- 
-- * the module can be flagged as being pure Haskell
-- 
-- * the correct suffix will automagically be appended
--
dumpCHS                       :: String -> CHSModule -> Bool -> CST s ()
dumpCHS fname mod pureHaskell  =
  do
    let (suffix, kind) = if pureHaskell
			 then (hssuffix , "(Haskell)")
			 else (chssuffix, "(C->HS binding)")
    (version, _, _) <- getId
    writeFileCIO (fname ++ suffix) (contents version kind)
  where
    contents version kind = 
      "-- GENERATED by " ++ version ++ " " ++ kind ++ "\n\
      \-- ** Edit the orignal .chs file instead!\n\n"
      ++ showCHSModule mod pureHaskell

-- convert a CHS module into a string
--
-- * if the second argument is `True', it is all Haskell code
--
showCHSModule                               :: CHSModule -> Bool -> String
showCHSModule (CHSModule frags) pureHaskell  = 
  (  foldr (.) id 
   . map (showCHSFrag pureHaskell) 
   $ frags) ""

showCHSFrag :: Bool -> CHSFrag -> ShowS
showCHSFrag _     (CHSVerb s   ) = showString s
showCHSFrag False (CHSHook hook) =   showString "{#" 
				   . showCHSHook hook
				   . showString "#}"
showCHSFrag True  (CHSHook _   ) = interr "showCHSFrag: Illegal hook!"

showCHSHook :: CHSHook -> ShowS
showCHSHook (CHSImport isQual ide _ _) =   
    showString "import "
  . (if isQual then showString "qualified " else id)
  . showCHSIdent ide
showCHSHook (CHSContext oheader olib oprefix _) =   
    showString "context "
  . (case oheader of
       Nothing     -> showString ""
       Just header ->   showString "header = " 
		      . showString header 
		      . showString " ")
  . (case olib of
       Nothing  -> showString ""
       Just lib -> showString "lib = " . showString lib . showString " ")
  . showPrefix oprefix False
showCHSHook (CHSType ide _) =   
    showString "type "
  . showCHSIdent ide
showCHSHook (CHSSizeof ide _) =   
    showString "sizeof "
  . showCHSIdent ide
showCHSHook (CHSEnum ide oalias trans oprefix derive _) =   
    showString "enum "
  . showIdAlias ide oalias
  . showCHSTrans trans
  . showPrefix oprefix True
  . if null derive then id else showString $
      "deriving (" 
      ++ concat (intersperse ", " (map identToLexeme derive))
      ++ ") "
showCHSHook (CHSCall isPure isUns ide oalias _) =   
    showString "call "
  . (if isPure then showString "pure " else id)
  . (if isUns then showString "unsafe " else id)
  . showIdAlias ide oalias
showCHSHook (CHSFun isPure isUns ide oalias octxt parms parm _) =   
    showString "fun "
  . (if isPure then showString "pure " else id)
  . (if isUns then showString "unsafe " else id)
  . showIdAlias ide oalias
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
showCHSHook (CHSPointer star ide oalias ptrType isNewtype oRefType _) =
    showString "pointer "
  . (if star then showString "*" else showString "")
  . showIdAlias ide oalias
  . (case ptrType of
       CHSForeignPtr -> showString " foreign"
       CHSStablePtr  -> showString " stable"
       _	     -> showString "")
  . (case (isNewtype, oRefType) of
       (True , _       ) -> showString " newtype" 
       (False, Just ide) -> showString " -> " . showCHSIdent ide
       (False, Nothing ) -> showString "")
showCHSHook (CHSClass oclassIde classIde typeIde _) =   
    showString "class "
  . (case oclassIde of
       Nothing       -> showString ""
       Just classIde -> showCHSIdent classIde . showString " => ")
  . showCHSIdent classIde
  . showString " "
  . showCHSIdent typeIde

showPrefix                        :: Maybe String -> Bool -> ShowS
showPrefix Nothing       _         = showString ""
showPrefix (Just prefix) withWith  =   maybeWith 
				     . showString "prefix = " 
				     . showString prefix 
				     . showString " "
  where
    maybeWith = if withWith then showString "with " else id

showIdAlias            :: Ident -> Maybe Ident -> ShowS
showIdAlias ide oalias  =
    showCHSIdent ide
  . (case oalias of
       Nothing  -> id
       Just ide -> showString " as " . showCHSIdent ide)

showCHSParm                                                :: CHSParm -> ShowS
showCHSParm (CHSParm oimMarsh hsTyStr twoCVals oomMarsh _)  =
    showOMarsh oimMarsh
  . showChar ' '
  . showHsVerb hsTyStr
  . (if twoCVals then showChar '&' else id)
  . showChar ' '
  . showOMarsh oomMarsh
  where
    showOMarsh Nothing               = id
    showOMarsh (Just (ide, argKind)) =   showCHSIdent ide
				       . (case argKind of
					   CHSValArg  -> id
					   CHSIOArg   -> showString "*"
					   CHSVoidArg -> showString "-")
    --
    showHsVerb str = showChar '`' . showString str . showChar '\''

showCHSTrans                          :: CHSTrans -> ShowS
showCHSTrans (CHSTrans _2Case assocs)  =   
    showString "{"
  . (if _2Case then showString ("underscoreToCase" ++ maybeComma) else id)
  . foldr (.) id (intersperse (showString ", ") (map showAssoc assocs))
  . showString "}"
  where
    maybeComma = if null assocs then "" else ", "
    --
    showAssoc (ide1, ide2) =
	showCHSIdent ide1
      . showString " as "
      . showCHSIdent ide2

showCHSAPath :: CHSAPath -> ShowS
showCHSAPath (CHSRoot ide) =
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
showCHSIdent  = showString . identToLexeme


-- load and dump a CHI file
-- ------------------------

chisuffix :: String
chisuffix  = ".chi"

versionPrefix :: String
versionPrefix  = "C->Haskell Interface Version "

-- load a CHI file (EXPORTED)
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
		   let fullnames = [path ++ '/':fname ++ chisuffix | 
				    path <- paths]
		   fullname <- findFirst fullnames
		     (fatal $ fname++chisuffix++" not found in:\n"++
			      unlines paths)
		   -- read file
		   --
		   traceInfoRead fullname
		   contents <- readFileCIO fullname

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
		     
		   (version, _, _) <- getId
		   let Just (myMajor, myMinor) = majorMinor version
		   when (major /= myMajor || minor /= myMinor) $
		     errorCHIVersion fname 
		       (major ++ "." ++ minor) (myMajor ++ "." ++ myMinor)

		   -- finalize
		   --
		   traceInfoOK
		   return $ concat chi
		  where
		    traceInfoRead fname = putTraceStr tracePhasesSW
					    ("Attempting to read file `"
					     ++ fname ++ "'...\n")
		    traceInfoVersion    = putTraceStr tracePhasesSW 
					    ("...checking version `" 
					     ++ fname ++ "'...\n")
		    traceInfoOK         = putTraceStr tracePhasesSW
					    ("...successfully loaded `"
					     ++ fname ++ "'.\n")
		    findFirst []        err =  err
		    findFirst (p:aths)  err =  do
		      e <- doesFileExistCIO p
		      if e then return p else findFirst aths err
		 

-- given a file name (no suffix) and a CHI file, the information is printed 
-- into that file (EXPORTED)
-- 
-- * the correct suffix will automagically be appended
--
dumpCHI                :: String -> String -> CST s ()
dumpCHI fname contents  =
  do
    (version, _, _) <- getId
    writeFileCIO (fname ++ chisuffix) $
      versionPrefix ++ version ++ "\n" ++ contents

-- extract major and minor number from a version string
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

-- alternative action in case of a syntax exception
--
ifError                :: CST s a -> CST s a -> CST s a
ifError action handler  = action `catchExc` (syntaxExc, const handler)

-- raise syntax error exception
--
raiseSyntaxError :: CST s a
raiseSyntaxError  = throwExc syntaxExc "syntax error"

-- parse a complete module
--
-- * errors are entered into the compiler state
--
parseCHSModule        :: Position -> String -> CST s CHSModule
parseCHSModule pos cs  = do
			   toks <- lexCHS cs pos
			   frags <- parseFrags toks
			   return (CHSModule frags)

-- parsing of code fragments
--
-- * in case of an error, all tokens that are neither Haskell nor control
--   tokens are skipped; afterwards parsing continues
--
parseFrags      :: [CHSToken] -> CST s [CHSFrag]
parseFrags toks  = do
		     parseFrags0 toks
		     `ifError` contFrags toks
  where
    parseFrags0 :: [CHSToken] -> CST s [CHSFrag]
    parseFrags0 []                       = return []
    parseFrags0 (CHSTokHaskell _ s:toks) = do
					     frags <- parseFrags toks
					     return (CHSVerb s : frags)
    parseFrags0 (CHSTokCtrl    _ c:toks) = do
					     frags <- parseFrags toks
					     return (CHSVerb [c] : frags)
    parseFrags0 (CHSTokImport  pos:toks) = parseImport  pos        toks
    parseFrags0 (CHSTokContext pos:toks) = parseContext pos        toks
    parseFrags0 (CHSTokType    pos:toks) = parseType    pos        toks
    parseFrags0 (CHSTokSizeof  pos:toks) = parseSizeof  pos        toks
    parseFrags0 (CHSTokEnum    pos:toks) = parseEnum    pos        toks
    parseFrags0 (CHSTokCall    pos:toks) = parseCall    pos        toks
    parseFrags0 (CHSTokFun     pos:toks) = parseFun     pos        toks
    parseFrags0 (CHSTokGet     pos:toks) = parseField   pos CHSGet toks
    parseFrags0 (CHSTokSet     pos:toks) = parseField   pos CHSSet toks
    parseFrags0 (CHSTokClass   pos:toks) = parseClass   pos        toks
    parseFrags0 (CHSTokPointer pos:toks) = parsePointer pos        toks
    parseFrags0 toks			 = syntaxError toks
    --
    -- skip to next Haskell or control token
    --
    contFrags      []                       = return []
    contFrags toks@(CHSTokHaskell _ _:_   ) = parseFrags toks
    contFrags toks@(CHSTokCtrl    _ _:_   ) = parseFrags toks
    contFrags      (_                :toks) = contFrags  toks

parseImport :: Position -> [CHSToken] -> CST s [CHSFrag]
parseImport pos toks = do
  (qual, modid, toks') <- 
    case toks of
      CHSTokIdent _ ide                :toks -> return (False, ide, toks)
      CHSTokQualif _: CHSTokIdent _ ide:toks -> return (True , ide, toks)
      _					     -> syntaxError toks
  chi <- loadCHI . identToLexeme $ modid
  toks'' <- parseEndHook toks'
  frags <- parseFrags toks''
  return $ CHSHook (CHSImport qual modid chi pos) : frags

parseContext          :: Position -> [CHSToken] -> CST s [CHSFrag]
parseContext pos toks  = do
		           (oheader , toks' )  <- parseOptHeader       toks
		           (olib    , toks'' ) <- parseOptLib          toks'
		           (opref   , toks''') <- parseOptPrefix False toks''
		           toks''''	       <- parseEndHook	       toks'''
		           frags               <- parseFrags           toks''''
			   let frag = CHSContext oheader olib opref pos
		           return $ CHSHook frag : frags

parseType :: Position -> [CHSToken] -> CST s [CHSFrag]
parseType pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSType ide pos) : frags
parseType _ toks = syntaxError toks

parseSizeof :: Position -> [CHSToken] -> CST s [CHSFrag]
parseSizeof pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSSizeof ide pos) : frags
parseSizeof _ toks = syntaxError toks

parseEnum :: Position -> [CHSToken] -> CST s [CHSFrag]
parseEnum pos (CHSTokIdent _ ide:toks) =
  do
    (oalias, toks' )   <- parseOptAs ide True toks
    (trans , toks'')   <- parseTrans          toks'
    (oprefix, toks''') <- parseOptPrefix True toks''
    (derive, toks'''') <- parseDerive	      toks'''
    toks'''''	       <- parseEndHook	      toks''''
    frags              <- parseFrags	      toks'''''
    return $ CHSHook (CHSEnum ide (norm oalias) trans oprefix derive pos) : frags
  where
    norm Nothing                   = Nothing
    norm (Just ide') | ide == ide' = Nothing
		     | otherwise   = Just ide'
parseEnum _ toks = syntaxError toks

parseCall          :: Position -> [CHSToken] -> CST s [CHSFrag]
parseCall pos toks  = 
  do
    (isPure  , toks'   ) <- parseIsPure          toks
    (isUnsafe, toks''  ) <- parseIsUnsafe        toks'
    (ide     , toks''' ) <- parseIdent           toks''
    (oalias  , toks'''') <- parseOptAs ide False toks'''
    toks'''''		 <- parseEndHook	 toks''''
    frags                <- parseFrags		 toks'''''
    return $ 
      CHSHook (CHSCall isPure isUnsafe ide (norm ide oalias) pos) : frags

parseFun          :: Position -> [CHSToken] -> CST s [CHSFrag]
parseFun pos toks  = 
  do
    (isPure  , toks' ) <- parseIsPure          toks
    (isUnsafe, toks'2) <- parseIsUnsafe        toks'
    (ide     , toks'3) <- parseIdent           toks'2
    (oalias  , toks'4) <- parseOptAs ide False toks'3
    (octxt   , toks'5) <- parseOptContext      toks'4
    (parms   , toks'6) <- parseParms	       toks'5
    (parm    , toks'7) <- parseParm	       toks'6
    toks'8	       <- parseEndHook	       toks'7
    frags              <- parseFrags	       toks'8
    return $ 
      CHSHook 
        (CHSFun isPure isUnsafe ide (norm ide oalias) octxt parms parm pos) :
      frags
  where
    parseOptContext (CHSTokHSVerb _ ctxt:CHSTokDArrow _:toks) =
      return (Just ctxt, toks)
    parseOptContext toks				      =
      return (Nothing  , toks)
    --
    parseParms (CHSTokLBrace _:CHSTokRBrace _:CHSTokArrow _:toks) = 
      return ([], toks)
    parseParms (CHSTokLBrace _                             :toks) = 
      parseParms' (CHSTokComma nopos:toks)
    parseParms						    toks  = 
      syntaxError toks
    --
    parseParms' (CHSTokRBrace _:CHSTokArrow _:toks) = return ([], toks)
    parseParms' (CHSTokComma _               :toks) = do
      (parm , toks' ) <- parseParm   toks
      (parms, toks'') <- parseParms' toks'
      return (parm:parms, toks'')
    parseParms' (CHSTokRBrace _              :toks) = syntaxError toks
      -- gives better error messages
    parseParms'				      toks  = syntaxError toks

parseIsPure :: [CHSToken] -> CST s (Bool, [CHSToken])
parseIsPure (CHSTokPure _:toks) = return (True , toks)
parseIsPure (CHSTokFun  _:toks) = return (True , toks)  -- backwards compat.
parseIsPure toks		= return (False, toks)
-- FIXME: eventually, remove `fun'; it's currently deprecated

parseIsUnsafe :: [CHSToken] -> CST s (Bool, [CHSToken])
parseIsUnsafe (CHSTokUnsafe _:toks) = return (True , toks)
parseIsUnsafe toks		    = return (False, toks)

norm :: Ident -> Maybe Ident -> Maybe Ident
norm ide Nothing                   = Nothing
norm ide (Just ide') | ide == ide' = Nothing
		     | otherwise   = Just ide'

parseParm :: [CHSToken] -> CST s (CHSParm, [CHSToken])
parseParm toks =
  do
    (oimMarsh, toks' ) <- parseOptMarsh toks
    (hsTyStr, twoCVals, pos, toks'2) <- 
      case toks' of
	(CHSTokHSVerb pos hsTyStr:CHSTokAmp _:toks'2) -> 
	  return (hsTyStr, True , pos, toks'2)
	(CHSTokHSVerb pos hsTyStr	     :toks'2) -> 
	  return (hsTyStr, False, pos, toks'2)
	toks				              -> syntaxError toks
    (oomMarsh, toks'3) <- parseOptMarsh toks'2
    return (CHSParm oimMarsh hsTyStr twoCVals oomMarsh pos, toks'3)
  where
    parseOptMarsh :: [CHSToken] -> CST s (Maybe (Ident, CHSArg), [CHSToken])
    parseOptMarsh (CHSTokIdent _ ide:CHSTokStar _ :toks) = 
      return (Just (ide, CHSIOArg) , toks)
    parseOptMarsh (CHSTokIdent _ ide:CHSTokMinus _:toks) = 
      return (Just (ide, CHSVoidArg), toks)
    parseOptMarsh (CHSTokIdent _ ide              :toks) = 
      return (Just (ide, CHSValArg) , toks)
    parseOptMarsh toks					 =
      return (Nothing, toks)

parseField :: Position -> CHSAccess -> [CHSToken] -> CST s [CHSFrag]
parseField pos access toks =
  do
    (path, toks') <- parsePath  toks
    frags         <- parseFrags toks'
    return $ CHSHook (CHSField access path pos) : frags

parsePointer :: Position -> [CHSToken] -> CST s [CHSFrag]
parsePointer pos toks =
  do
    (isStar, ide, toks')          <- 
      case toks of
	CHSTokStar _:CHSTokIdent _ ide:toks' -> return (True , ide, toks')
	CHSTokIdent _ ide             :toks' -> return (False, ide, toks')
	_				     -> syntaxError toks
    (oalias , toks'2)             <- parseOptAs ide True toks'
    (ptrType, toks'3)             <- parsePtrType        toks'2
    let 
     (isNewtype, oRefType, toks'4) =
      case toks'3 of
	CHSTokNewtype _                  :toks' -> (True , Nothing , toks' )
	CHSTokArrow   _:CHSTokIdent _ ide:toks' -> (False, Just ide, toks' )
	_				        -> (False, Nothing , toks'3)
    toks'5	                  <- parseEndHook toks'4
    frags                         <- parseFrags   toks'5
    return $ 
      CHSHook 
       (CHSPointer isStar ide (norm ide oalias) ptrType isNewtype oRefType pos)
       : frags
  where
    parsePtrType :: [CHSToken] -> CST s (CHSPtrType, [CHSToken])
    parsePtrType (CHSTokForeign _:toks) = return (CHSForeignPtr, toks)
    parsePtrType (CHSTokStable _ :toks) = return (CHSStablePtr, toks)
    parsePtrType                  toks	= return (CHSPtr, toks)

    norm ide Nothing                   = Nothing
    norm ide (Just ide') | ide == ide' = Nothing
			 | otherwise   = Just ide'

parseClass :: Position -> [CHSToken] -> CST s [CHSFrag]
parseClass pos (CHSTokIdent  _ sclassIde:
	        CHSTokDArrow _          :
		CHSTokIdent  _ classIde :
		CHSTokIdent  _ typeIde  :
		toks)                     =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSClass (Just sclassIde) classIde typeIde pos) : frags
parseClass pos (CHSTokIdent _ classIde :
		CHSTokIdent _ typeIde  :
		toks)                     =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSClass Nothing classIde typeIde pos) : frags
parseClass _ toks = syntaxError toks

parseOptHeader :: [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptHeader (CHSTokHeader _    :
	        CHSTokEqual  _    :
	        CHSTokString _ str:
	        toks)	             = return (Just str, toks)
parseOptHeader (CHSTokHeader _:toks) = syntaxError toks
parseOptHeader toks		     = return (Nothing, toks)

parseOptLib :: [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptLib (CHSTokLib    _    :
	     CHSTokEqual  _    :
	     CHSTokString _ str:
	     toks)	          = return (Just str, toks)
parseOptLib (CHSTokLib _:toks	) = syntaxError toks
parseOptLib toks		  = return (Nothing, toks)

parseOptPrefix :: Bool -> [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptPrefix False (CHSTokPrefix _    :
		      CHSTokEqual  _    :
		      CHSTokString _ str:
		      toks)	           = return (Just str, toks)
parseOptPrefix True  (CHSTokWith   _    :
		      CHSTokPrefix _    :
		      CHSTokEqual  _    :
		      CHSTokString _ str:
	              toks)	           = return (Just str, toks)
parseOptPrefix _     (CHSTokWith   _:toks) = syntaxError toks
parseOptPrefix _     (CHSTokPrefix _:toks) = syntaxError toks
parseOptPrefix _     toks		   = return (Nothing, toks)

-- first argument is the identifier that is to be used when `^' is given and
-- the second indicates whether the first characters has to be upper case
--
parseOptAs :: Ident -> Bool -> [CHSToken] -> CST s (Maybe Ident, [CHSToken])
parseOptAs _   _     (CHSTokAs _:CHSTokIdent _ ide:toks) = 
  return (Just ide, toks)
parseOptAs ide upper (CHSTokAs _:CHSTokHat pos    :toks) = 
  return (Just $ underscoreToCase ide upper pos, toks)
parseOptAs _   _     (CHSTokAs _                  :toks) = syntaxError toks
parseOptAs _   _				   toks	 = 
  return (Nothing, toks)

-- convert C style identifier to Haskell style identifier
--
underscoreToCase               :: Ident -> Bool -> Position -> Ident
underscoreToCase ide upper pos  = 
  let lexeme = identToLexeme ide
      ps     = filter (not . null) . parts $ lexeme
  in
  onlyPosIdent pos . adjustHead . concat . map adjustCase $ ps
  where
    parts s = let (l, s') = break (== '_') s
	      in  
	      l : case s' of
		    []      -> []
		    (_:s'') -> parts s''
    --    
    adjustCase (c:cs) = toUpper c : map toLower cs
    --
    adjustHead ""     = ""
    adjustHead (c:cs) = if upper then toUpper c : cs else toLower c:cs

-- this is disambiguated and left factored
--
parsePath :: [CHSToken] -> CST s (CHSAPath, [CHSToken])
parsePath (CHSTokStar pos:toks) =
  do
    (path, toks') <- parsePath toks
    return (CHSDeref path pos, toks')
parsePath (CHSTokIdent _ ide:toks) =
  do
    (pathWithHole, toks') <- parsePath' toks
    return (pathWithHole (CHSRoot ide), toks')
parsePath toks = syntaxError toks

-- `s->m' is represented by `(*s).m' in the tree
--
parsePath' :: [CHSToken] -> CST s (CHSAPath -> CHSAPath, [CHSToken])
parsePath' (CHSTokDot _:CHSTokIdent _ ide:toks) =
  do
    (pathWithHole, toks') <- parsePath' toks
    return (pathWithHole . (\hole -> CHSRef hole ide), toks')
parsePath' (CHSTokDot _:toks) = 
  syntaxError toks
parsePath' (CHSTokArrow pos:CHSTokIdent _ ide:toks) =
  do
    (pathWithHole, toks') <- parsePath' toks
    return (pathWithHole . (\hole -> CHSRef (CHSDeref hole pos) ide), toks')
parsePath' (CHSTokArrow _:toks) = 
  syntaxError toks
parsePath' toks =
  do
    toks' <- parseEndHook toks
    return (id, toks')

parseTrans :: [CHSToken] -> CST s (CHSTrans, [CHSToken])
parseTrans (CHSTokLBrace _:toks) =
  do
    (_2Case, toks' ) <- parse_2Case toks
    case toks' of
      (CHSTokRBrace _:toks'') -> return (CHSTrans _2Case [], toks'')
      _			      ->
        do
	  -- if there was no `underscoreToCase', we add a comma token to meet
	  -- the invariant of `parseTranss'
	  --
	  (transs, toks'') <- if _2Case 
			      then parseTranss toks'
			      else parseTranss (CHSTokComma nopos:toks')
          return (CHSTrans _2Case transs, toks'')
  where
    parse_2Case (CHSTok_2Case _:toks) = return (True, toks)
    parse_2Case toks		      = return (False, toks)
    --
    parseTranss (CHSTokRBrace _:toks) = return ([], toks)
    parseTranss (CHSTokComma  _:toks) = do
					  (assoc, toks' ) <- parseAssoc toks
					  (trans, toks'') <- parseTranss toks'
					  return (assoc:trans, toks'')
    parseTranss toks		      = syntaxError toks
    --
    parseAssoc (CHSTokIdent _ ide1:CHSTokAs _:CHSTokIdent _ ide2:toks) =
      return ((ide1, ide2), toks)
    parseAssoc (CHSTokIdent _ ide1:CHSTokAs _:toks                   ) =
      syntaxError toks
    parseAssoc (CHSTokIdent _ ide1:toks                              ) =
      syntaxError toks
    parseAssoc toks						       =
      syntaxError toks
parseTrans toks = syntaxError toks

parseDerive :: [CHSToken] -> CST s ([Ident], [CHSToken])
parseDerive (CHSTokDerive _ :CHSTokLParen _:CHSTokRParen _:toks) = 
  return ([], toks)
parseDerive (CHSTokDerive _ :CHSTokLParen _:toks)                = 
  parseCommaIdent (CHSTokComma nopos:toks)
  where
    parseCommaIdent :: [CHSToken] -> CST s ([Ident], [CHSToken])
    parseCommaIdent (CHSTokComma _:CHSTokIdent _ ide:toks) =
      do
        (ids, tok') <- parseCommaIdent toks
        return (ide:ids, tok')
    parseCommaIdent (CHSTokRParen _                 :toks) = 
      return ([], toks)
parseDerive toks = return ([],toks)

parseIdent :: [CHSToken] -> CST s (Ident, [CHSToken])
parseIdent (CHSTokIdent _ ide:toks) = return (ide, toks)
parseIdent toks			    = syntaxError toks

parseEndHook :: [CHSToken] -> CST s ([CHSToken])
parseEndHook (CHSTokEndHook _:toks) = return toks
parseEndHook toks		    = syntaxError toks

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

errorCHINotFound     :: String -> CST s a
errorCHINotFound ide  = do
  raiseError nopos 
    ["Unknown .chi file!",
     "Cannot find the .chi file for `" ++ ide ++ "'."]
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
