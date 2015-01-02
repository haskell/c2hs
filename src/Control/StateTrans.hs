--  The HiPar Toolkit: state transformer routines
--
--  Author : Manuel M. T. Chakravarty
--  Created: 3 March 95
--
--  Copyright (C) [1995..1999] Manuel M. T. Chakravarty
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
--  This module provides basic support for the use of state transformers.
--  The state transformer is build around the `IO' monad to allow the
--  manipulation of external state. It encapsulated two separate states with
--  the intention to use the first one for the omnipresent compiler state
--  consisting of the accumulated error messages etc. and to use the second as
--  a generic component that can be used in different ways by the different
--  phases of the compiler.
--
--  The module also supports the use of exceptions and fatal errors.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * We explicitly do not use any names for the monad types and functions
--    that are used by either Haskell's `IO' monad or GHC's `ST' monad.  Since
--    Haskell 1.4, `STB' is an instance of the `Monad' constructor class.
--
--  * To integrate the Haskell prelude `IO' monad into our `STB' monad we use
--    the technique from ``Composing monads'' by Mark P. Jones and Luc
--    Duponcheel (Report YALEU/DCS/RR-1004) from 1993, Section 8.
--
--  * The use of GHC's inplace-update goodies within monads of kind `STB' is
--    possible, bacause `IO' is based on `ST' in the GHC.
--
--  * In the following, we call the two kinds of state managed by the `STB' the
--    base state (the omnipresent state of the compiler) and generic state.
--
--  * `STB' is a newtype, which requires careful wrapping and unwrapping of its
--    values in the following definitions.
--
--- TODO ----------------------------------------------------------------------
--
--  * with constructor classes, the state transformer business can be made
--    more elegant (they weren't around when this module was initially written)
--
--  * it would be possible to maintain the already applied changes to the base
--    and generic state even in the case of a fatal error, when in `listIO'
--    every IO operation is encapsulated into a handler that transforms IO
--    errors into exceptions
--

module Control.StateTrans (-- the monad and the generic operations
                   --
                   STB,
                   --
                   -- monad specific operations
                   --
                   readBase, writeBase, transBase, readGeneric, writeGeneric,
                   transGeneric, liftIO, runSTB, interleave,
                   --
                   -- exception handling and fatal errors
                   --
                   throwExc, fatal, catchExc, fatalsHandledBy)
where


import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)
import Control.Exception (catch)
import Prelude hiding (catch)

-- BEWARE! You enter monad country. Read any of Wadler's or
-- Launchbury/Peyton-Jones' texts before entering. Otherwise,
-- your mental health my be in danger.  You have been warned!


-- state transformer base and its monad operations
-- -----------------------------------------------

-- | the generic form of a state transformer using the external state represented
-- by 'IO'; 'STB' is a abbreviation for state transformer base
--
-- the first state component @bs@ is provided for the omnipresent compiler
-- state and the, second, @gs@ for the generic component
--
-- the third component of the result distinguishes between erroneous and
-- successful computations where
--
--   @Left (tag, msg)@ -- stands for an exception identified by @tag@ with
--                        error message @msg@, and
--   @Right a@         -- is a successfully delivered result
--
newtype STB bs gs a = STB (bs -> gs -> IO (bs, gs, Either (String, String) a))

instance Functor (STB bs gs) where
  fmap = liftM

instance Applicative (STB bs gs) where
  pure  = return
  (<*>) = ap

instance Monad (STB bs gs) where
  return = yield
  (>>=)  = (+>=)

-- | the monad's unit
--
yield   :: a -> STB bs gs a
yield a  = STB $ \bs gs -> return (bs, gs, Right a)

-- | the monad's bind
--
-- * exceptions are propagated
--
(+>=)   :: STB bs gs a -> (a -> STB bs gs b) -> STB bs gs b
m +>= k  = let
             STB m' = m
           in
           STB $ \bs gs -> m' bs gs >>= \(bs', gs', res) ->
                     case res of
                       Left  exc -> return (bs', gs', Left exc)  -- prop exc
                       Right a   -> let
                                      STB k' = k a
                                    in
                                    k' bs' gs'                   -- cont


-- generic state manipulation
-- --------------------------

-- base state:
--

-- | given a reader function for the base state, wrap it into an STB monad
--
readBase   :: (bs -> a) -> STB bs gs a
readBase f  = STB $ \bs gs -> return (bs, gs, Right (f bs))

-- | given a new base state, inject it into an STB monad
--
writeBase     :: bs -> STB bs gs ()
writeBase bs'  = STB $ \_ gs -> return (bs', gs, Right ())

-- | given a transformer function for the base state, wrap it into an STB monad
--
transBase   :: (bs -> (bs, a)) -> STB bs gs a
transBase f  = STB $ \bs gs -> let
                                 (bs', a) = f bs
                               in
                                 return (bs', gs, Right a)

-- generic state:
--

-- | given a reader function for the generic state, wrap it into an STB monad
--
readGeneric   :: (gs -> a) -> STB bs gs a
readGeneric f  = STB $ \bs gs -> return (bs, gs, Right (f gs))

-- | given a new generic state, inject it into an STB monad
--
writeGeneric     :: gs -> STB bs gs ()
writeGeneric gs'  = STB $ \bs _ -> return (bs, gs', Right ())

-- | given a transformer function for the generic state, wrap it into an STB
-- monad
--
transGeneric   :: (gs -> (gs, a)) -> STB bs gs a
transGeneric f  = STB $ \bs gs -> let
                                    (gs', a) = f gs
                                  in
                                  return (bs, gs', Right a)


-- interaction with the encapsulated 'IO' monad
-- --------------------------------------------

-- | lifts an 'IO' state transformer into 'STB'
--
liftIO   :: IO a -> STB bs gs a
liftIO m  = STB $ \bs gs -> m >>= \r -> return (bs, gs, Right r)

-- | given an initial state, executes the 'STB' state transformer yielding an
-- 'IO' state transformer that must be placed into the context of the external
-- IO
--
-- * uncaught exceptions become fatal errors
--
runSTB         :: STB bs gs a -> bs -> gs -> IO a
runSTB m bs gs  = let
                    STB m' = m
                  in
                  m' bs gs >>= \(_, _, res) ->
                  case res of
                    Left  (tag, msg) -> let
                                          err = userError ("Exception `"
                                                           ++ tag ++ "': "
                                                           ++ msg)
                                        in
                                        ioError err
                    Right a          -> return a

-- | interleave the (complete) execution of an 'STB' with another generic state
-- component into an 'STB'
--
interleave :: STB bs gs' a -> gs' -> STB bs gs a
interleave m gs' = STB $ let
                           STB m' = m
                         in
                         \bs gs
                         -> (m' bs gs' >>= \(bs', _, a) -> return (bs', gs, a))


-- error and exception handling
-- ----------------------------

-- * we exploit the 'UserError' component of 'IOError' for fatal errors
--
-- * we distinguish exceptions and user-defined fatal errors
--
--   - exceptions are meant to be caught in order to recover the currently
--     executed operation; they turn into fatal errors if they are not caught;
--     execeptions are tagged, which allows to deal with multiple kinds of
--     execeptions at the same time and to handle them differently
--   - user-defined fatal errors abort the currently executed operation, but
--     they may be caught at the top-level in order to terminate gracefully or
--     to invoke another operation; there is no special support for different
--     handling of different kinds of fatal-errors
--
-- * the costs for fatal error handling are already incurred by the 'IO' monad;
--   the costs for exceptions mainly is the case distinction in the definition
--   of '+>='
--

-- | throw an exception with the given tag and message
--
throwExc         :: String -> String -> STB bs gs a
throwExc tag msg  = STB $ \bs gs -> return (bs, gs, Left (tag, msg))

-- | raise a fatal user-defined error
--
-- * such an error my be caught and handled using 'fatalsHandeledBy'
--
fatal   :: String -> STB bs gs a
fatal s  = liftIO (ioError (userError s))

-- | the given state transformer is executed and exceptions with the given tag
-- are caught using the provided handler, which expects to get the exception
-- message
--
-- * the base and generic state observed by the exception handler is *modified*
--   by the failed state transformer upto the point where the exception was
--   thrown (this semantics is the only reasonable when it should be possible
--   to use updating for maintaining the state)
--
catchExc                  :: STB bs gs a
                          -> (String, String -> STB bs gs a)
                          -> STB bs gs a
catchExc m (tag, handler)  =
  STB $ \bs gs
        -> let
             STB m' = m
           in
           m' bs gs >>= \state@(bs', gs', res) ->
           case res of
             Left (tag', msg) -> if (tag == tag')       -- exception with...
                                 then
                                   let
                                     STB handler' = handler msg
                                   in
                                   handler' bs' gs'     -- correct tag, catch
                                 else
                                   return state         -- wrong tag, rethrow
             Right _          -> return state           -- no exception

-- | given a state transformer that may raise fatal errors and an error handler
-- for fatal errors, execute the state transformer and apply the error handler
-- when a fatal error occurs
--
-- * fatal errors are IO monad errors and errors raised by 'fatal' as well as
--   uncaught exceptions
--
-- * the base and generic state observed by the error handler is *in contrast
--   to 'catch'* the state *before* the state transformer is applied
--
fatalsHandledBy :: STB bs gs a -> (IOError -> STB bs gs a) -> STB bs gs a
fatalsHandledBy m handler  =
  STB $ \bs gs
        -> (let
              STB m' = m
            in
            m' bs gs >>= \state@(_gs', _bs', res) ->
            case res of
              Left  (tag, msg) -> let
                                    err = userError ("Exception `" ++ tag
                                                     ++ "': " ++ msg)
                                  in
                                  ioError err
              Right _a         -> return state
            )
            `catch` (\err -> let
                               STB handler' = handler err
                             in
                             handler' bs gs)
