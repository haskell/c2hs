--  C->Haskell Compiler: custom header generator
--
--  Author : Manuel M T Chakravarty
--  Created: 5 February 2003
--
--  Copyright (c) 2004 Manuel M T Chakravarty
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
--  This module implements the generation of a custom header from a binding
--  module.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Computing CPP Conditionals
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~
--  We obtain information about which branches of CPP conditions are taken
--  during pre-processing of the custom header file by introducing new
--  struct declarations.  Specifically, after each #if[[n]def] or #elif,
--  we place a declaration of the form
--
--    struct C2HS_COND_SENTRY<unique number>;
--
--  We can, then, determine which branch of a conditional has been taken by
--  checking whether the struct corresponding to that conditional has been
--  declared.
--
--- TODO ----------------------------------------------------------------------
--
-- * Ideally, `ghFrag[s]' should be tail recursive

module C2HS.Gen.Header (
  genHeader
) where

-- standard libraries
import Control.Monad (when,liftM)

-- Language.C / Compiler Toolkit
import Language.C.Data
import Language.C.Pretty
import Language.C.Syntax
import Data.Errors       (interr)
import Data.DList (DList)
import qualified Data.DList as DL

-- C->Haskell
import C2HS.State (CST, runCST, transCST, raiseError, catchExc,
                  throwExc, errorsPresent, showErrors, fatal)

-- friends
import C2HS.CHS  (CHSModule(..), CHSFrag(..), CHSHook(..), CHSChangeCase(..),
                  CHSTrans(..), CHSAPath(..))


-- | The header generation monad
--
type GH a = CST [Name] a

-- | Generate a custom C header from a CHS binding module.
--
-- * All CPP directives and inline-C fragments are moved into the custom header
--
-- * The CPP and inline-C fragments are removed from the .chs tree and
--   conditionals are replaced by structured conditionals
--
genHeader :: CHSModule -> CST s ([String], CHSModule, String)
genHeader mod' =
  do
    (header, mod'') <- runCST (ghModule mod') newNameSupply
                     `ifGHExc` return ([], CHSModule [])
    -- check for errors and finalise
    --
    errs <- errorsPresent
    if errs
      then do
        errmsgs <- showErrors
        fatal ("Errors during generation of C header:\n\n"   -- fatal error
               ++ errmsgs)
      else do
        warnmsgs <- showErrors
        return (header, mod'', warnmsgs)

-- | Obtain a new base name that may be used, in C, to encode the result of a
-- preprocessor conditional.
--
newName :: CST [Name] String
newName = transCST $
  \supply -> (tail supply, "C2HS_COND_SENTRY_" ++ (show.nameId) (head supply))

-- | Various forms of processed fragments
--
data FragElem = Frag  CHSFrag
              | Elif  String Position
              | Else  Position
              | Endif Position
              | EOF

instance Pos FragElem where
  posOf (Frag frag    ) = posOf frag
  posOf (Elif _    pos) = pos
  posOf (Else      pos) = pos
  posOf (Endif     pos) = pos
  posOf EOF             = nopos

-- | check for end of file
--
isEOF :: FragElem -> Bool
isEOF EOF = True
isEOF _   = False

-- | Generate the C header for an entire .chs module.
--
-- * This works more or less like a recursive decent parser for a statement
--   sequence that may contain conditionals, where 'ghFrag' implements most of
--   the state transition system of the associated automaton
--
ghModule :: CHSModule -> GH ([String], CHSModule)
ghModule (CHSModule frags) =
  do
    (header, frags', last', _rest) <- ghFrags frags
    when (not . isEOF $ last') $
      notOpenCondErr (posOf last')
    return (DL.toList header, CHSModule frags')

-- | Collect header and fragments up to eof or a CPP directive that is part of a
-- conditional
--
-- * We collect the header (ie, CPP directives and inline-C) using a
--   difference list to avoid worst case O(n^2) complexity due to
--   concatenation of lines that go into the header.
--
ghFrags :: [CHSFrag] -> GH (DList String, [CHSFrag], FragElem, [CHSFrag])
ghFrags []    = return (DL.empty, [], EOF, [])
ghFrags frags =
  do
    (header, frag, rest) <- ghFrag frags
    case frag of
      Frag aFrag -> do
                      (header2, frags', frag', rest') <- ghFrags rest
                      -- FIXME: Not tail rec
                      return (header `DL.append` header2, aFrag:frags',
                              frag', rest')
      _          -> return (header, [], frag, rest)

-- | Process a single fragment *structure*; i.e., if the first fragment
-- introduces a conditional, process the whole conditional; otherwise, process
-- the first fragment
--
ghFrag :: [CHSFrag] -> GH (DList String, -- partial header file
                           FragElem,     -- processed fragment
                           [CHSFrag])    -- not yet processed fragments
ghFrag []                              =
  return (DL.empty, EOF, [])
ghFrag (frag@(CHSVerb  _ _  ) : frags) =
  return (DL.empty, Frag frag, frags)

-- generate an
--   enum __c2hs__enum__'id { __c2hs_enr__'id = DEF1, ... }
-- and then process an ordinary enum directive
ghFrag (_frag@(CHSHook (CHSEnumDefine hsident trans
                        instances pos) hkpos) : frags) =
  do ide <- newEnumIdent
     (enrs,trans') <- createEnumerators trans
     return (DL.fromList [show.pretty $ enumDef ide enrs,";\n"],
             Frag (enumFrag (identToString ide) trans'), frags)
  where
  newEnumIdent = liftM internalIdent $ transCST $
                 \supply -> (tail supply, "__c2hs_enum__" ++
                                          show (nameId $ head supply))
  newEnrIdent  = liftM internalIdent $ transCST $
                 \supply -> (tail supply, "__c2hs_enr__" ++
                                          show (nameId $ head supply))
  createEnumerators (CHSTrans isUnderscore changeCase aliases omits)
    | isUnderscore =
      raiseErrorGHExc pos ["underScoreToCase is meaningless " ++
                           "for `enum define' hooks"]
    | changeCase /= CHSSameCase =
        raiseErrorGHExc pos ["changing case is meaningless " ++
                             "for `enum define' hooks"]
    | otherwise =
      do (enrs,transtbl') <- liftM unzip (mapM createEnumerator aliases)
         return (enrs,CHSTrans False CHSSameCase transtbl' omits)
  createEnumerator (cid,hsid) =
    liftM (\enr -> ((enr,cid),(enr,hsid))) newEnrIdent
  enumDef ide enrs = CEnum (Just ide) (Just$ map mkEnr enrs) [] undefNode
    where mkEnr (name,value) = (name, Just $ CVar value undefNode)
  enumFrag ide trans' = CHSHook (CHSEnum (internalIdent ide) (Just hsident)
                                 trans' True Nothing Nothing
                                 instances pos) hkpos

ghFrag (_frag@(CHSHook (CHSConst cident pos) hkpos) : frags) =
  do ide <- newConstIdent
     return (DL.fromList [show.pretty $ constDef ide,";\n"],
             Frag (CHSHook (CHSConst ide pos) hkpos), frags)
  where
  newConstIdent =
    liftM internalIdent $ transCST $
    \supply -> (tail supply, "__c2hs__const__" ++ show (nameId $ head supply))
  constDef ide =
    -- This is a little nasty.  We write a definition of an *integer*
    -- C value into the header, regardless of what type it really
    -- is...
    CDecl [CTypeSpec (CIntType undefNode)]
          [(Just (CDeclr (Just ide) [] Nothing [] undefNode),
            Just (CInitExpr (CVar cident undefNode) undefNode),
            Nothing)]
          undefNode

ghFrag (frag@(CHSHook (CHSFun _ _ True varTypes
                       (CHSRoot _ ide) oalias _ _ _ _) _) : frags) = do
  let ideLexeme = identToString ide
      hsLexeme  = ideLexeme `maybe` identToString $ oalias
      vaIdent base idx = "__c2hs__vararg__" ++ base ++ "_" ++ show idx
      ides = map (vaIdent hsLexeme) [0..length varTypes - 1]
      defs = zipWith (\t i -> t ++ " " ++ i ++ ";\n") varTypes ides
  return (DL.fromList defs, Frag frag, frags)

ghFrag (frag@(CHSHook (CHSTypedef cIde hsIde _) _) : frags) = do
  let cTypLexeme = identToString cIde
      hsTypLexeme = identToString hsIde
      defs = [cTypLexeme ++ " __c2hs_typedef__" ++
              cTypLexeme ++ "__" ++ hsTypLexeme ++ ";\n"]
  return (DL.fromList defs, Frag frag, frags)

ghFrag (frag@(CHSHook  _    _) : frags) = return (DL.empty, Frag frag, frags)
ghFrag (frag@(CHSLine  _    ) : frags) = return (DL.empty, Frag frag, frags)
ghFrag (     (CHSC    s  _  ) : frags) =
  do
    (header, frag, frags' ) <- ghFrag frags     -- scan for next CHS fragment
    return (DL.singleton s `DL.append` header, frag, frags')
    -- FIXME: this is not tail recursive...

ghFrag (     (CHSCond _  _  ) : _    ) =
  interr "GenHeader.ghFrags: There can't be a structured conditional yet!"
ghFrag (     (CHSCPP s pos nl) : frags) =
  let
    (directive, _) =   break (`elem` " \t")
                     . dropWhile (`elem` " \t")
                     $ s
  in
  case directive of
    "if"     -> openIf s pos frags
    "ifdef"  -> openIf s pos frags
    "ifndef" -> openIf s pos frags
    "else"   -> return (DL.empty                 , Else   pos           , frags)
    "elif"   -> return (DL.empty                 , Elif s pos           , frags)
    "endif"  -> return (DL.empty                 , Endif  pos           , frags)
    _        -> return (DL.fromList ['#':s, "\n"],
                        Frag (CHSVerb (if nl then "\n" else "") pos), frags)
  where
    -- enter a new conditional (may be an #if[[n]def] or #elif)
    --
    -- * Arguments are the lexeme of the directive `s', the position of that
    --   directive `pos', and the fragments following the directive `frags'
    --
    openIf s' _pos frags' =
      do
        (headerTh, fragsTh, last', rest) <- ghFrags frags'
        case last' of
          Else    _pos' -> do
                           (headerEl, fragsEl, last'', rest') <- ghFrags rest
                           case last'' of
                             Else    pos' -> notOpenCondErr pos'
                             Elif  _ pos' -> notOpenCondErr pos'
                             Endif   _    -> closeIf
                                              ((headerTh
                                                `DL.snoc` "#else\n")
                                               `DL.append`
                                               (headerEl
                                                `DL.snoc` "#endif\n"))
                                              (s', fragsTh)
                                              []
                                              (Just fragsEl)
                                              rest'
                             EOF         -> notClosedCondErr pos
          Elif s'' pos' -> do
                           (headerEl, condFrag, rest') <- openIf s'' pos' rest
                           case condFrag of
                             Frag (CHSCond alts dft) ->
                               closeIf (headerTh `DL.append` headerEl)
                                       (s, fragsTh)
                                       alts
                                       dft
                                       rest'
                             _                       ->
                               interr "GenHeader.ghFrag: Expected CHSCond!"
          Endif   _   -> closeIf (headerTh `DL.snoc` "#endif\n")
                                 (s', fragsTh)
                                 []
                                 (Just [])
                                 rest
          EOF         -> notClosedCondErr pos
    --
    -- turn a completed conditional into a 'CHSCond' fragment
    --
    -- * `(s, fragsTh)' is the CPP directive `s' containing the condition under
    --   which `fragTh' should be executed; `alts' are alternative branches
    --   (with conditions); and `oelse' is an optional else-branch
    --
    closeIf headerTail (s', fragsTh) alts oelse rest =
      do
        sentryName <- newName
        let sentry = internalIdent sentryName
                       -- don't use an internal ident, as we need to test for
                       -- equality with identifiers read from the .i file
                       -- during binding hook expansion
            header = DL.fromList ['#':s', "\n",
                             "struct ", sentryName, ";\n"]
                            `DL.append` headerTail
        return (header, Frag (CHSCond ((sentry, fragsTh):alts) oelse), rest)


-- exception handling
-- ------------------

-- | exception identifier
--
ghExc :: String
ghExc  = "ghExc"

-- | throw an exception
--
throwGHExc :: GH a
throwGHExc  = throwExc ghExc "Error during C header generation"

-- | catch a 'ghExc'
--
ifGHExc           :: CST s a -> CST s a -> CST s a
ifGHExc m handler  = m `catchExc` (ghExc, const handler)

-- | raise an error followed by throwing a GH exception
--
raiseErrorGHExc          :: Position -> [String] -> GH a
raiseErrorGHExc pos errs  = raiseError pos errs >> throwGHExc


-- error messages
-- --------------

notClosedCondErr :: Position -> GH a
notClosedCondErr pos  =
  raiseErrorGHExc pos
    ["Unexpected end of file!",
     "File ended while the conditional block starting here was not closed \
     \properly."]

notOpenCondErr :: Position -> GH a
notOpenCondErr pos  =
  raiseErrorGHExc pos
    ["Missing #if[[n]def]!",
     "There is a #else, #elif, or #endif without an #if, #ifdef, or #ifndef."]
