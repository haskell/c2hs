--  C -> Haskell Compiler: Lexer for C Header Files
--
--  Author : Manuel M T Chakravarty, Duncan Coutts
--  Created: 12 Febuary 2007
--
--  Copyright (c) [1999..2004] Manuel M T Chakravarty
--  Copyright (c) 2005-2007 Duncan Coutts
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
--  Monad for the C lexer and parser
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  This monad has to be usable with Alex and Happy. Some things in it are
--  dictated by that, eg having to be able to remember the last token.
--
--  The monad also provides a unique name supply (via the Names module)
--
--  For parsing C we have to maintain a set of identifiers that we know to be
--  typedef'ed type identifiers. We also must deal correctly with scope so we
--  keep a list of sets of identifiers so we can save the outer scope when we
--  enter an inner scope.
--
--- TODO ----------------------------------------------------------------------
--
--

module CParserMonad ( 
  P, 
  execParser,
  failP,
  getNewName,        -- :: P Name
  addTypedef,        -- :: Ident -> P ()
  shadowTypedef,     -- :: Ident -> P ()
  isTypeIdent,       -- :: Ident -> P Bool
  enterScope,        -- :: P ()
  leaveScope,        -- :: P ()
  setPos,            -- :: Position -> P ()
  getPos,            -- :: P Position
  getInput,          -- :: P String
  setInput,          -- :: String -> P ()
  getLastToken,      -- :: P CToken
  setLastToken,      -- :: CToken -> P ()
  ) where

import Position  (Position(..), Pos(posOf))
import Errors    (interr)
import UNames	 (Name)
import Idents    (Ident, lexemeToIdent, identToLexeme)

import Data.Set  (Set)
import qualified Data.Set as Set (fromList, insert, member, delete)

import CTokens (CToken)

data ParseResult a
  = POk !PState a
  | PFailed [String] Position	-- The error message and position

data PState = PState { 
        curPos     :: !Position,	-- position at current input location
        curInput   :: !String,		-- the current input
        prevToken  ::  CToken,		-- the previous token
        namesupply :: ![Name],		-- the name unique supply
        tyidents   :: !(Set Ident),	-- the set of typedef'ed identifiers
        scopes     :: ![Set Ident]	-- the tyident sets for outer scopes
     }

newtype P a = P { unP :: PState -> ParseResult a }

instance Monad P where
  return = returnP
  (>>=) = thenP
  fail m = getPos >>= \pos -> failP pos [m]

execParser :: P a -> String -> Position -> [Ident] -> [Name]
           -> Either a ([String], Position)
execParser (P parser) input pos builtins names =
  case parser initialState of
    POk _ result -> Left result
    PFailed message pos -> Right (message, pos)
  where initialState = PState {
          curPos = pos,
          curInput = input,
          prevToken = interr "CLexer.execParser: Touched undefined token!",
          namesupply = names,
          tyidents = Set.fromList builtins,
          scopes   = []
        }

{-# INLINE returnP #-}
returnP :: a -> P a
returnP a = P $ \s -> POk s a

{-# INLINE thenP #-}
thenP :: P a -> (a -> P b) -> P b
(P m) `thenP` k = P $ \s ->
	case m s of
		POk s' a        -> (unP (k a)) s'
		PFailed err pos -> PFailed err pos

failP :: Position -> [String] -> P a
failP pos msg = P $ \_ -> PFailed msg pos

getNewName :: P Name
getNewName = P $ \s@PState{namesupply=(n:ns)} -> POk s{namesupply=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{curPos=pos} ()

getPos :: P Position
getPos = P $ \s@PState{curPos=pos} -> POk s pos

addTypedef :: Ident -> P ()
addTypedef ident = (P $ \s@PState{tyidents=tyidents} ->
                             POk s{tyidents = ident `Set.insert` tyidents} ())

shadowTypedef :: Ident -> P ()
shadowTypedef ident = (P $ \s@PState{tyidents=tyidents} ->
                             -- optimisation: mostly the ident will not be in
                             -- the tyident set so do a member lookup to avoid
                             --  churn induced by calling delete
                             POk s{tyidents = if ident `Set.member` tyidents
                                                then ident `Set.delete` tyidents
                                                else tyidents } ())

isTypeIdent :: Ident -> P Bool
isTypeIdent ident = P $ \s@PState{tyidents=tyidents} ->
                             POk s $! Set.member ident tyidents

enterScope :: P ()
enterScope = P $ \s@PState{tyidents=tyidents,scopes=ss} ->
                     POk s{scopes=tyidents:ss} ()

leaveScope :: P ()
leaveScope = P $ \s@PState{scopes=ss} ->
                     case ss of
                       []             -> interr "leaveScope: already in global scope"
                       (tyidents:ss') -> POk s{tyidents=tyidents, scopes=ss'} ()

getInput :: P String
getInput = P $ \s@PState{curInput=i} -> POk s i

setInput :: String -> P ()
setInput i = P $ \s -> POk s{curInput=i} ()

getLastToken :: P CToken
getLastToken = P $ \s@PState{prevToken=tok} -> POk s tok

setLastToken :: CToken -> P ()
setLastToken tok = P $ \s -> POk s{prevToken=tok} ()
