--  Compiler Toolkit: Self-optimizing LL(1) parser combinators
--
--  Author : Manuel M. T. Chakravarty
--  Created: 27 February 99
--
--  Version $Revision: 1.22 $ from $Date: 2001/02/09 02:36:10 $
--
--  Copyright (c) [1999..2000] Manuel M. T. Chakravarty
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Library General Public
--  License as published by the Free Software Foundation; either
--  version 2 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Library General Public License for more details.
--
--- DESCRIPTION ---------------------------------------------------------------
--
--  This module implements fully deterministic, self-optimizing LL(1) parser
--  combinators, which generate parse tables on-the-fly and are based on a
--  technique introduced by Swierstra & Duponcheel.  The applied technique for
--  efficiently computing the parse tables makes essential use of the
--  memorization build into lazy evaluation.
--
--  The present implementation is rather different from S. D. Swierstra and
--  L. Duponcheel, ``Deterministic, Error-Correcting Combinator Parsers'', in
--  John Launchbury, Erik Meijer, and Tim Sheard (Eds.) "Advanced Functional
--  Programming", Springer-Verlag, Lecture Notes in Computer Science 1129,
--  184-207, 1996.  It is much closer to a a revised version published by
--  S. D. Swierstra, but handles actions completely different.  In particular, 
--  Swierstra's version does not have a threaded state and meta actions.  The
--  present module also defines a number of additional combinators and uses
--  finite maps to optimise the construction of the transition relation stored 
--  in the node of the transition graph (this also saves substantial memory).
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98 & rank-2 polymorphism (existentially quantified type 
--	      variables)
--
--  Unlike conventional parser combinators, the combinators do not produce
--  parsers, but only specifications of parsers that can then be executed
--  using the function `parse'.
--
--  It is basically impossible to get this efficient without universally-
--  quantified data type fields (or existentially quantified type variables)
--  as soon as we encode the parsers in a data structure.  The reason is that
--  we cannot store the action functions in the structure without that
--  feature.
--
--  A user-defined state can be passed down in the parser and be threaded
--  through the individual actions.
--
--  Tokens:
--
--  * Tokens must contain a position and equality as well as an ordering
--    relation must be defined for them.  The equality determines whether
--    tokens "match" during parsing, ie, whether they are equal modulo their
--    attributes (the position is, of course, an attribute).  The ordering
--    supports an optimised representation of the transition graph.  Tokens
--    are, furthermore, printable (instance of `Show'); the resulting string
--    should correspond to the lexeme of the token and not the data
--    constructor used to represent it internally.
--
--  * I tried using arrays to represent the transition relation in the nodes
--    of the graph, but this leads to an enormous memory consumption (at least 
--    with ghc 4.05).  One reason for this is certainly that these arrays are
--    relatively sparsely populated.
--
--- TODO ----------------------------------------------------------------------
--
--  * Error correction is still missing.
--

module Parsers (Token, Parser, empty, token, skip, (<|>), (*$>), (*>), ($>),
		action, meta, opt, (-*>), (*->), many, list, many1, list1,
		sep, seplist, sep1, seplist1, execParser)
where

import List       (sort)

import Common     (Position, Pos (posOf), nopos)
import FiniteMaps (FiniteMap, unitFM, joinCombFM, mapFM, lookupFM, toListFM)
import Errors     (interr, ErrorLvl(..), Error, makeError)

infix  5 `opt`
infixl 4 *>, -*>, *->, *$>, $>
infixl 3 `action`
infixl 2 <|>


-- data structures
-- ---------------

-- token class (EXPORTED)
--
class (Pos t, Show t, Eq t, Ord t) => Token t

-- tree structure used to represent parsers specifications (EXPORTED
-- ABSTRACTLY) 
--
-- * each node corresponds to a state of the represented automaton and is
--   composed out of an action and a parsing continuation, which encodes the
--   state transition function in the current state
--
data Token t =>
     Parser a t r = forall q. Parser (Action a t q r)	-- action functions
				     (Cont a t q)       -- parsing continuation

-- parsing continuation
--
data Token t =>
     Cont a t r = -- maybe end of input
		  --
		  Empty r			-- return if no input
			(Parser a t r)		-- used if there is input
		  --
		  -- selection of acceptable tokens paired with following 
		  -- parser state
		  --
		| Alts (FiniteMap t (Parser a t r))
		  --
		  -- represents an automaton without any transitions
		  -- (semantically equivalent to `Alts zeroFM', but easier to
		  -- match)
		  --
		| Done

-- actions
--
-- * Note that the rank-2 polymorphism (existentially quantified type 
--   variable) is essential here to seperate the action function from the
--   parser (if we don't do that, the actions are pushed down in the parser
--   structure until they reach the `Empty' variant matching the end-of-file
--   in the actual parse - this makes the parser structure as deep as the
--   input has tokens!)
--
-- * the meta action is transforming a threaded state top-down; the result of
--   the state transformer (type `q'') is passed to the tree constructor
--   action, after the following parser has been applied; the meta action has
--   to be executed before the parser is applied, as the parser get's the
--   internal state *after* transformed by the meta action; overall we have,
--   (1) meta action, (2) recursive application of the parser, (3) tree
--   constructing action
--
data Token t =>
     Action a t q r = forall q'. Action (a -> (a, q'))		-- meta action
					(q' -> t -> q -> r)	-- tree constr

-- internal tree constructors
--

nometa              :: Token t => (t -> q -> r) -> Action a t q r
nometa simpleAction  = Action (\s -> (s, ())) (\_ -> simpleAction)

singleton     :: Token t => t -> Parser a t r -> Cont a t r
singleton t p  = Alts $ unitFM t p

noaction :: Token t => Cont a t q -> Parser a t q
noaction  = Parser $ nometa (flip const)

tokaction :: Token t => Cont a t q -> Parser a t t
tokaction  = Parser $ nometa const

noparser :: Token t => Parser a t r
noparser  = noaction Done


-- basic combinators
-- -----------------

-- Without consuming any input, yield the given result value (EXPORTED)
--
empty   :: Token t => r -> Parser a t r
empty x  = noaction $ Empty x noparser

-- Consume a token that is equal to the given one; the consumed token is
-- returned as the result (EXPORTED) 
--
token   :: Token t => t -> Parser a t t
token t  = tokaction $ singleton t (empty ())

-- Consume a token that is equal to the given one; the consumed token is
-- thrown away (EXPORTED) 
--
skip   :: Token t => t -> Parser a t ()
skip t  = noaction $ singleton t (empty ())

-- Alternative parsers (EXPORTED)
--
(<|>) :: Token t => Parser a t r -> Parser a t r -> Parser a t r
--
-- * Alternatives require to merge the alternative sets of the two parsers.
--   The most interesting case is where both sets contain cases for the same
--   token.  In this case, we left factor over this token.  This requires some 
--   care with the actions, because we have to be able to decide which of
--   the two actions to apply.  To do so, the two parsers prefix their results 
--   with a `Left' or `Right' tag, which makes it easy to decided in the new
--   combined action, which of the two subparsers did match.
--
(Parser _ Done)         <|> q                        = q
p                       <|> (Parser _ Done)          = p
--(Parser a (Empty _ p))  <|> (Parser a' (Empty _ q))  = grammarErr p q
(Parser a (Empty x p))  <|> q                        = mergeEpsilon a  x p q
p                       <|> (Parser a' (Empty x q))  = mergeEpsilon a' x q p
(Parser a (Alts alts1)) <|> (Parser a' (Alts alts2)) = 
  Parser (a `joinActions` a') $ Alts (joinCombFM (<|>) alts1' alts2')
  where
    alts1' = mapFM (\_ p -> Left  $> p) alts1
    alts2' = mapFM (\_ p -> Right $> p) alts2

grammarErr     :: Token t => Parser a t r -> Parser a t r -> b
grammarErr p q  = interr $ "Parsers.<|>: Ambiguous grammar!\n\
			   \  first (left  parser): " ++ first p ++ "\n\
			   \  first (right parser): " ++ first q ++ "\n"

mergeEpsilon :: Token t
	     => Action a t q r -> q -> Parser a t q -> Parser a t r 
	     -> Parser a t r
mergeEpsilon a x p q = 
  let anew   = a `joinActions` nometa (flip const)  -- mustn't touch token!
      newcon = Empty (Left x) (Left $> p <|> Right $> q)
  in
  Parser anew newcon 

joinActions :: Token t 
	    => Action a t q r -> Action a t q' r 
	    -> Action a t (Either q q') r
(Action m con) `joinActions` (Action m' con') =
  Action (joinMeta m m')
	 (\(q'1, q'2) t qalt -> case qalt of
				  Left  q -> con  q'1 t q
				  Right q -> con' q'2 t q)

-- combine two meta action into one, which yields a pair of the individual
-- results (the state is threaded through one after another - no assumption
-- may be made about the order)
--
joinMeta :: (a -> (a, r1)) -> (a -> (a, r2)) -> a -> (a, (r1, r2))
joinMeta meta meta' = \s -> let 
			      (s' , q'1) = meta  s
			      (s'', q'2) = meta' s'
			    in 
			    (s'', (q'1, q'2))

-- Sequential parsers, where the result of the first is applied to the result
-- of the second (EXPORTED)
--
(*$>) :: Token t => Parser a t (s -> r) -> Parser a t s -> Parser a t r
-- !!!
(Parser a@(Action m con) Done) *$> q = 
  let con' = interr "Parsers.(*$>): Touched action after an error!"
  in
  Parser (Action m con') Done
(Parser a@(Action m con) (Empty f p)) *$> q = 
--  _scc_ "*$>:Empty"
  let a' = Action m (\q' t q -> con q' t f q)
  in
  contract a p *$> q <|> contract a' q
(Parser (Action m con) (Alts alts)) *$> q = 
--  _scc_ "*$>:Alt"
  let con' x' t (xp, xq) = con x' t xp xq
  in
  Parser (Action m con') (Alts $ mapFM (\_ p -> p *> q) alts)

contract :: Token t => Action a t q r -> Parser a t q -> Parser a t r
contract (Action m con) (Parser (Action m' con') c) =
  let a' = Action (joinMeta m m')
		  (\(x'1, x'2) t x -> con x'1 notok (con' x'2 t x))
  in
  Parser a' c
  where
    notok = interr $ "Parsers.(*$>): Touched forbidden token!"

-- Sequential parsers, where the overall result is the pair of the component
-- results (EXPORTED)
--
(*>)   :: Token t => Parser a t s -> Parser a t r -> Parser a t (s, r)
p *> q  = (,) $> p *$> q

-- apply a function to the result yielded by a parser (EXPORTED)
--
($>) :: Token t => (s -> r) -> Parser a t s -> Parser a t r
f $> Parser (Action m con) c = let con' q' t q = f $ con q' t q
			       in
			       Parser (Action m con') c

-- produces a parser that encapsulates a meta action manipulating the
-- threaded state (EXPORTED)
--
meta :: Token t => (a -> (a, r)) -> Parser a t r
meta g  = Parser (Action g (\q' _ _ -> q')) (Empty () noparser)


-- non-basic combinators
-- ---------------------

-- postfix action (EXPORTED)
--
action :: Token t => Parser a t s -> (s -> r) -> Parser a t r
action  = flip ($>)

-- optional parse (EXPORTED)
--
opt       :: Token t => Parser a t r -> r -> Parser a t r
p `opt` r  = p <|> empty r

-- sequential composition, where the result of the rhs is discarded (EXPORTED)
--
(*->)   :: Token t => Parser a t r -> Parser a t s -> Parser a t r
p *-> q  = const $> p *$> q

-- sequential composition, where the result of the lhs is discarded (EXPORTED)
--
(-*>)   :: Token t => Parser a t s -> Parser a t r -> Parser a t r
p -*> q  = flip const $> p *$> q

-- accept a sequence of productions from a nonterminal (EXPORTED)
--
-- * Uses a graphical structure to require only constant space, but this
--   behaviour is destroyed if the replicated parser is a `skip c'.
--
many       :: Token t => (r -> s -> s) -> s -> Parser a t r -> Parser a t s
--
-- * we need to build a cycle, to avoid building the parser structure over and 
--   over again
--
many f e p  = let me = (f $> p *$> me) `opt` e
	      in me

-- return the results of a sequence of productions from a nonterminal in a
-- list (EXPORTED) 
--
list :: Token t => Parser a t r -> Parser a t [r]
list  = many (:) [] 

-- accept a sequence consisting of at least one production from a nonterminal
-- (EXPORTED) 
--
many1     :: Token t => (r -> r -> r) -> Parser a t r -> Parser a t r
--many1 f p = p <|> (f <$> p <*> many1 f p)
many1 f p = let me = p <|> (f $> p *$> me)
	    in me

-- accept a sequence consisting of at least one production from a nonterminal
-- and return a list of results (EXPORTED) 
--
list1   :: Token t => Parser a t r -> Parser a t [r]
list1 p  = let me =     (\x -> [x]) $> p 
		    <|> ((:) $> p *$> me)
	   in me

-- accept a sequence of productions from a nonterminal, which are seperated by 
-- productions of another nonterminal (EXPORTED)
--
sep :: Token t 
    => (r -> u -> s -> s) 
    -> (r -> s) 
    -> s 
    -> Parser a t u 
    -> Parser a t r 
    -> Parser a t s
sep f g e sepp p  = let me = g $> p <|> (f $> p *$> sepp *$> me)
		    in me `opt` e

-- return the results of a sequence of productions from a nonterminal, which
-- are seperated by productions of another nonterminal, in a list (EXPORTED)
--
seplist :: Token t => Parser a t s -> Parser a t r -> Parser a t [r]
seplist  = sep (\h _ l -> h:l) (\x -> [x]) [] 

-- accept a sequence of productions from a nonterminal, which are seperated by 
-- productions of another nonterminal (EXPORTED)
--
sep1 :: Token t 
     => (r -> s -> r -> r) -> Parser a t s -> Parser a t r -> Parser a t r
sep1 f sepp p  = let me = p <|> (f $> p *$> sepp *$> me)
		 in me

-- accept a sequence consisting of at least one production from a nonterminal, 
-- which are separated by the productions of another nonterminal; the list of
-- results is returned (EXPORTED)
--
seplist1        :: Token t => Parser a t s -> Parser a t r -> Parser a t [r]
seplist1 sepp p = p *> list (sepp -*> p) `action` uncurry (:)
{- Is the above also space save?  Should be.  Contributed by Roman.
seplist1 sepp p  = let me =     (\x -> [x]) $> p 
		            <|> ((:) $> p *-> sepp *$> me)
	           in me
-}


-- execution of a parser
-- ---------------------

-- apply a parser to a token sequence (EXPORTED)
--
-- * Trailing tokens are returned in the third component of the result (the
--   longest match is found).
--
-- * Currently, all errors are fatal; thus, the result (first component of the 
--   returned pair) is undefined in case of an error (this changes when error
--   correction is added).
--
execParser :: Token t => Parser a t r -> a -> [t] -> (r, [Error], [t])
--
-- * Regarding the case cascade in the second equation, note that laziness is
--   not our friend here.  The root of the parse tree will be constructed at
--   the very end of parsing; so, there is no way, we can have any pipelining
--   with following stages here (and then there are the error messages, which
--   also spoil pipelining).
--
execParser (Parser (Action m con) c) a [] =   -- eof
  case c of 
    Empty x _ -> (con (snd . m $ a) errtoken x, [], [])
    _         -> (errresult, [makeError FatalErr nopos eofErr], [])
execParser (Parser (Action m con) c) a ts =   -- eat one token
  case m a of				      --   execute meta action
    (a', x') -> case cont c a' ts of	      --   process next input token
-- !!!		  (t, (x, errs, ts')) -> ((((con $! x') $ t) $!x), errs, ts')
		  (t, (x, errs, ts')) -> ((((con $ x') $ t) $ x), errs, ts')
  where
    cont :: Token t => Cont a t r -> a -> [t] -> (t, (r, [Error], [t]))
    cont Done        _ (t:_)  = makeErr (posOf t) trailErr
    cont (Alts alts) a (t:ts) = case lookupFM alts t of
				  Nothing -> makeErr (posOf t) (illErr t)
				  Just p  -> (t, execParser p a ts)
    cont (Empty x p) a ts     =
      case p of
	Parser _ Done      -> (errtoken, (x, [], ts))
	_                  -> (errtoken, (execParser p a ts))

makeErr pos err = (errtoken, (errresult, [makeError FatalErr pos err], []))

eofErr   = ["Unexpected end of input!",
	    "The code at the end of the file seems truncated."]
trailErr = ["Trailing garbage!",
	    "There seem to be characters behind the valid end of input."]
illErr t = ["Syntax error!",
	    "The symbol `" ++ show t ++ "' does not fit here."]

errresult = interr "Parsers.errresult: Touched undefined result!"
errtoken  = interr "Parsers.errtoken: Touched undefined token!"


-- for debugging
-- -------------

-- first set of the given parser (prefixed by a `*' if this is an epsilon
-- parser) 
--
first :: Token t => Parser a t r -> String
first (Parser _ (Empty _ p))  = "*" ++ first p
first (Parser _ (Alts  alts)) =   show 
				. sort 
				. map show 
				. map fst 
				. toListFM 
				$ alts

instance Token t => Show (Parser a t r) where
  showsPrec _ (Parser a c) = shows c

instance Token t => Show (Cont a t r) where
  showsPrec _ (Empty r p ) = showString "*" . shows p
  showsPrec _ (Alts  alts) = shows alts
