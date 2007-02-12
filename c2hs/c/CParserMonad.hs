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
  setPos,            -- :: Position -> P ()
  getPos,            -- :: P Position
  addTypedef,        -- :: Ident -> P ()
  isTypeIdent,       -- :: Ident -> P Bool
  getInput,          -- :: P (Position, String)
  setInput,          -- :: (Position, String) -> P ()
  getLastToken,      -- :: P CToken
  setLastToken,      -- :: CToken -> P ()
  ) where

import Position  (Position(..), Pos(posOf))
import Errors    (interr)
import UNames	 (Name)
import Idents    (Ident, lexemeToIdent, identToLexeme)

import Data.Set  (Set)
import qualified Data.Set as Set (fromList, insert, member)

import CTokens (CToken)

data ParseResult a
  = POk !PState a
  | PFailed [String] Position	-- The error message and position

data PState = PState { 
	alex_pos :: !Position,	-- position at current input location
	alex_inp :: String,	-- the current input
	alex_last  :: CToken,	-- the previous token
	alex_names :: [Name],	-- the name unique supply
	alex_tdefs :: Set Ident	-- the set of typedef'ed identifiers
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
          alex_pos = pos,
	  alex_inp = input,
	  alex_last = interr "CLexer.execParser: Touched undefined token!",
	  alex_names = names,
	  alex_tdefs = Set.fromList builtins
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
getNewName = P $ \s@PState{alex_names=(n:ns)} -> POk s{alex_names=ns} n

setPos :: Position -> P ()
setPos pos = P $ \s -> POk s{alex_pos=pos} ()

getPos :: P Position
getPos = P $ \s@PState{alex_pos=pos} -> POk s pos

addTypedef :: Ident -> P ()
addTypedef ident = (P $ \s@PState{alex_tdefs=tdefs} ->
                             POk s{alex_tdefs = ident `Set.insert` tdefs} ())

isTypeIdent :: Ident -> P Bool
isTypeIdent ident = P $ \s@PState{alex_tdefs=tdefs} ->
                             POk s $! Set.member ident tdefs

getInput :: P (Position, String)
getInput = P $ \s@PState{alex_pos=p, alex_inp=i} -> POk s (p,i)

setInput :: (Position, String) -> P ()
setInput (p,i) = P $ \s -> POk s{alex_pos=p, alex_inp=i} ()

getLastToken :: P CToken
getLastToken = P $ \s@PState{alex_last=tok} -> POk s tok

setLastToken :: CToken -> P ()
setLastToken tok = P $ \s -> POk s{alex_last=tok} ()
