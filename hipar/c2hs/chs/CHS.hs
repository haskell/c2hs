--  C->Haskell Compiler: CHS file abstraction
--
--  Author : Manuel M. T. Chakravarty
--  Created: 16 August 99
--
--  Version $Revision: 1.11 $ from $Date: 2000/08/06 04:17:26 $
--
--  Copyright (c) 1999 Manuel M. T. Chakravarty
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
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  The following binding hooks are recognised:
--
--  hook    -> `{#' inner `#}'
--  inner   -> `context' ctxt
--	     | `type' ident
--	     | `enum' idalias trans
--	     | `call' [`fun'] [`unsafe'] idalias
--	     | `get' apath
--	     | `set' apath
--
--  ctxt    -> [`header' `=' string] [`lib' `=' string] [`prefix' `=' string]
--  idalias -> ident [`as' ident]
--  apath   -> ident
--	     | `*' apath
--	     | apath `.' ident
--	     | apath `->' ident
--  trans   -> `{' alias_1, ..., alias_n `}'
--  alias   -> `underscoreToCase'
--	     | ident `as' ident
--
--  If `underscoreToCase' occurs in a translation table, it must be the first
--  entry.
--
--  Remark: Optional Haskell names are normalised during structure tree
--	    construction, ie, associations that associated a name with itself
--	    are removed.  (They don't carry semantic content, and make some
--	    test more complicated.)
--
--- TODO ----------------------------------------------------------------------
--

module CHS (CHSModule(..), CHSFrag(..), CHSHook(..), CHSTrans(..),
	    CHSAccess(..), CHSAPath(..), loadCHS, dumpCHS, hssuffix, chssuffix)
where 

import Common    (Position, Pos(posOf), nopos)
import Errors	 (interr)
import Idents    (Ident, identToLexeme)

import C2HSState (CST, readFileCIO, writeFileCIO, getId, catchExc, throwExc,
		  raiseError, fatal, errorsPresent, showErrors,
		  Traces(..), putTraceStr) 
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
data CHSHook = CHSContext (Maybe String)	-- header name
			  (Maybe String)	-- library name
			  (Maybe String)	-- prefix
			  Position
	     | CHSType    Ident			-- C type
			  Position
	     | CHSEnum    Ident			-- C enumeration type
			  (Maybe Ident)		-- Haskell name
			  CHSTrans		-- translation table
			  Position
	     | CHSCall    Bool			-- is a pure function?
			  Bool			-- is unsafe?
			  Ident			-- C function
			  (Maybe Ident)		-- Haskell name
			  Position
	     | CHSField   CHSAccess		-- access type
			  CHSAPath		-- access path
			  Position

instance Pos CHSHook where
  posOf (CHSContext _ _ _   pos) = pos
  posOf (CHSType    _       pos) = pos
  posOf (CHSEnum    _ _ _   pos) = pos
  posOf (CHSCall    _ _ _ _ pos) = pos
  posOf (CHSField   _ _     pos) = pos

-- two hooks are equal if they have the same Haskell name and reference the
-- same C object 
--
instance Eq CHSHook where
  (CHSContext h1 olib1 opref1  _) == (CHSContext h2 olib2 opref2  _) =    
    h1 == h2 && olib1 == olib1 && opref1 == opref2
  (CHSType ide1                _) == (CHSType ide2                _) = 
    ide1 == ide2
  (CHSEnum ide1 oalias1 _      _) == (CHSEnum ide2 oalias2 _      _) = 
    oalias1 == oalias2 && ide1    == ide2
  (CHSCall _ _ ide1 oalias1    _) == (CHSCall _ _ ide2 oalias2    _) = 
    oalias1 == oalias2 && ide1    == ide2
  (CHSField acc1 path1         _) == (CHSField acc2 path2         _) =    
    acc1 == acc2 && path1 == path2
  _                               == _                          = False

-- translation table (EXPORTED)
--
data CHSTrans = CHSTrans Bool			-- underscore to case?
			 [(Ident, Ident)]	-- alias list

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
  . (case oprefix of
       Nothing   -> showString ""
       Just pref -> showString "prefix = " . showString pref . showString " ")
showCHSHook (CHSType ide _) =   
    showString "type "
  . showCHSIdent ide
showCHSHook (CHSEnum ide oalias trans _) =   
    showString "enum "
  . showCHSIdent ide
  . showString " "
  . (case oalias of
       Nothing  -> showString ""
       Just ide -> showString "as " . showCHSIdent ide . showString " ")
  . showCHSTrans trans
showCHSHook (CHSCall isFun isUns ide oalias _) =   
    showString "call "
  . (if isFun then showString "fun " else id)
  . (if isUns then showString "unsafe " else id)
  . showCHSIdent ide
  . (case oalias of
       Nothing  -> showString ""
       Just ide -> showString " as " . showCHSIdent ide)
showCHSHook (CHSField acc path _) =   
    (case acc of
       CHSGet -> showString "get "
       CHSSet -> showString "set ")
  . showCHSAPath path

showCHSTrans :: CHSTrans -> ShowS
showCHSTrans (CHSTrans _2Case assocs) =   
    showString "{"
  . (if _2Case then showString ("underscoreToCase" ++ maybeComma) else id)
  . showAssocs assocs _2Case
  . showString "}"
  where
    maybeComma = if null assocs then "" else ", "
    --
    showAssocs []             _      = id
    showAssocs (assoc:assocs) notFst =
	(if notFst then showString ", " else id)
      . showAssoc assoc
      . showAssocs assocs True
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
    parseFrags0 (CHSTokContext pos:toks) = parseContext pos        toks
    parseFrags0 (CHSTokType    pos:toks) = parseType    pos        toks
    parseFrags0 (CHSTokEnum    pos:toks) = parseEnum    pos        toks
    parseFrags0 (CHSTokCall    pos:toks) = parseCall    pos        toks
    parseFrags0 (CHSTokGet     pos:toks) = parseField   pos CHSGet toks
    parseFrags0 (CHSTokSet     pos:toks) = parseField   pos CHSSet toks
    parseFrags0 toks			 = syntaxError toks
    --
    -- skip to next Haskell or control token
    --
    contFrags      []                       = return []
    contFrags toks@(CHSTokHaskell _ _:_   ) = parseFrags toks
    contFrags toks@(CHSTokCtrl    _ _:_   ) = parseFrags toks
    contFrags      (_                :toks) = contFrags  toks

parseContext          :: Position -> [CHSToken] -> CST s [CHSFrag]
parseContext pos toks  = do
		           (oheader , toks' )  <- parseOptHeader toks
		           (olib    , toks'' ) <- parseOptLib    toks'
		           (opref   , toks''') <- parseOptPrefix toks''
		           toks''''	       <- parseEndHook   toks'''
		           frags               <- parseFrags     toks''''
			   let frag = CHSContext oheader olib opref pos
		           return $ CHSHook frag : frags

parseType :: Position -> [CHSToken] -> CST s [CHSFrag]
parseType pos (CHSTokIdent _ ide:toks) =
  do
    toks' <- parseEndHook toks
    frags <- parseFrags toks'
    return $ CHSHook (CHSType ide pos) : frags
parseType _ toks = syntaxError toks

parseEnum :: Position -> [CHSToken] -> CST s [CHSFrag]
parseEnum pos (CHSTokIdent _ ide:toks) =
  do
    (oalias, toks' ) <- parseOptAs   toks
    (trans , toks'') <- parseTrans   toks'
    toks'''	     <- parseEndHook toks''
    frags            <- parseFrags   toks'''
    return $ CHSHook (CHSEnum ide (norm oalias) trans pos) : frags
  where
    norm Nothing                   = Nothing
    norm (Just ide') | ide == ide' = Nothing
		     | otherwise   = Just ide'
parseEnum _ toks = syntaxError toks

parseCall          :: Position -> [CHSToken] -> CST s [CHSFrag]
parseCall pos toks  = 
  do
    (isFun   , toks'   ) <- parseIsFun    toks
    (isUnsafe, toks''  ) <- parseIsUnsafe toks'
    (ide     , toks''' ) <- parseIdent    toks''
    (oalias  , toks'''') <- parseOptAs    toks'''
    toks'''''		 <- parseEndHook  toks''''
    frags                <- parseFrags    toks'''''
    return $ CHSHook (CHSCall isFun isUnsafe ide (norm ide oalias) pos) : frags
  where
    parseIsFun (CHSTokFun _:toks) = return (True, toks)
    parseIsFun toks		  = return (False, toks)

    parseIsUnsafe (CHSTokUnsafe _:toks) = return (True, toks)
    parseIsUnsafe toks		        = return (False, toks)

    norm ide Nothing                   = Nothing
    norm ide (Just ide') | ide == ide' = Nothing
			 | otherwise   = Just ide'

parseField :: Position -> CHSAccess -> [CHSToken] -> CST s [CHSFrag]
parseField pos access toks =
  do
    (path, toks') <- parsePath  toks
    frags         <- parseFrags toks'
    return $ CHSHook (CHSField access path pos) : frags

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

parseOptPrefix :: [CHSToken] -> CST s (Maybe String, [CHSToken])
parseOptPrefix (CHSTokPrefix _    :
	        CHSTokEqual  _    :
	        CHSTokString _ str:
	        toks)	             = return (Just str, toks)
parseOptPrefix (CHSTokPrefix _:toks) = syntaxError toks
parseOptPrefix toks		     = return (Nothing, toks)

parseOptAs :: [CHSToken] -> CST s (Maybe Ident, [CHSToken])
parseOptAs (CHSTokAs _:CHSTokIdent _ ide:toks) = return (Just ide, toks)
parseOptAs (CHSTokAs _:toks		     ) = syntaxError toks
parseOptAs  toks			       = return (Nothing, toks)

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
    parse_2Case toks		       = return (False, toks)
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
