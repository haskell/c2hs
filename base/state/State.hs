--  Compiler Toolkit: compiler state management
--
--  Author : Manuel M. T. Chakravarty
--  Created: 2 November 95
--
--  Version $Revision: 1.30 $ from $Date: 2000/02/28 06:28:59 $
--
--  Copyright (c) [1995..1999] Manuel M. T. Chakravarty
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
--  This module forms the interface to the state base of the compiler. It is
--  used by all modules that are not directly involved in implementing the
--  state base. It provides a state transformer that is capable of doing I/O
--  and provides facilities such as error handling and compiler switch
--  management. 
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * The monad `PreCST' is reexported abstractly.
--
--  * Errors are dumped to `stdout' to facilitate communication with other
--    processes (see `Interact').
--
--- TODO ----------------------------------------------------------------------
--

module State (-- the PreCST monad
	      --
	      PreCST,					   -- reexport ABSTRACT
	      nop, yield, (+>=), (+>), fixCST,             -- reexport
	      throwExc, fatal, catchExc, fatalsHandledBy,  -- reexport lifted
	      readCST, writeCST, transCST, run, runCST, 
	      StateTrans.MVar, {-StateTrans.MArr, -}		   -- reexport
	      newMV, readMV, assignMV, {-newMA, readMA,	   -- reexport lifted
	      writeMA, StateTrans.boundsMA, -}		   -- reexport lifted
	      --
	      -- reexport compiler I/O
	      --
	      module CIO,
	      liftIO,
	      --
	      -- identification
	      --
	      getId,
	      --
	      -- error management
	      --
	      raise, raiseWarning, raiseError, raiseFatal, showErrors,
	      errorsPresent,
	      --
	      -- extra state management
	      --
	      readExtra, updExtra,
	      --
	      -- name supplies
	      --
	      getNameSupply)
where

import Ix
import Monad       (when)

import BaseVersion (version, copyright, disclaimer)
import Config	   (errorLimit)
import Common      (Position)
import Utils	   (sort)
import UNames      (NameSupply,
	            rootSupply, splitSupply)
import StateTrans  (STB,
		    readBase, transBase, runSTB)
import qualified
       StateTrans  (interleave, throwExc, fatal, catchExc, fatalsHandledBy, 
		    MVar, {-MArr,-}
		    newMV, readMV, assignMV{-, newMA, readMA, writeMA, boundsMA-})
import StateBase   (PreCST(..), ErrorState(..), BaseState(..),
		    nop, yield, (+>=), (+>), fixCST,
		    unpackCST, readCST, writeCST, transCST,
		    liftIO)
import CIO
import Errors      (ErrorLvl(..), Error, makeError, errorLvl, showError)


-- state used in the whole compiler
-- --------------------------------

-- initialization 
--
-- * it gets the version information and the initial extra state as arguments
--
initialBaseState        :: (String, String, String) -> e -> BaseState e
initialBaseState vcd es  = BaseState {
		             idTKBS     = (version, copyright, disclaimer),
			     idBS       = vcd,
			     errorsBS   = initialErrorState, 
			     suppliesBS = splitSupply rootSupply,
			     extraBS    = es
			}


-- executing state transformers
-- ----------------------------

-- initiate a complete run of the ToolKit represented by a PreCST with a void
-- generic component (type `()') (EXPORTED)
--
-- * fatals errors are explicitly caught and reported (instead of letting them
--   through to the runtime system)
--
run            :: (String, String, String) -> e -> PreCST e () a -> IO a
run vcd es cst  = runSTB m (initialBaseState vcd es) ()
  where
    m = unpackCST (
	  cst
	  `fatalsHandledBy` \err ->
	    putStrCIO ("Uncaught fatal error: " ++ show err)	>>
	    exitWithCIO (ExitFailure 1)
	)

-- run a PreCST in the context of another PreCST (EXPORTED)
--
-- the generic state of the enclosing PreCST is preserved while the
-- computation of the PreCST passed as an argument is interleaved in the
-- execution of the enclosing one 
--
runCST     :: PreCST e s a -> s -> PreCST e s' a
runCST m s  = CST $ StateTrans.interleave (unpackCST m) s


-- exception handling
-- ------------------

-- throw an exception with the given tag and message (EXPORTED)
--
throwExc       :: String -> String -> PreCST e s a
throwExc s1 s2  = CST $ StateTrans.throwExc s1 s2

-- raise a fatal user-defined error (EXPORTED)
--
-- * such an error my be caught and handled using `fatalsHandeledBy'
--
fatal :: String -> PreCST e s a
fatal  = CST . StateTrans.fatal

-- the given state transformer is executed and exceptions with the given tag
-- are caught using the provided handler, which expects to get the exception
-- message (EXPORTED)
--
-- * the state observed by the exception handler is *modified* by the failed
--   state transformer upto the point where the exception was thrown (this
--   semantics is the only reasonable when it should be possible to use
--   updating for maintaining the state)
--
catchExc     :: PreCST e s a 
	     -> (String, String -> PreCST e s a) 
	     -> PreCST e s a
catchExc m (s, h)  = CST $ StateTrans.catchExc (unpackCST m) (s, unpackCST . h)

-- given a state transformer that may raise fatal errors and an error handler
-- for fatal errors, execute the state transformer and apply the error handler
-- when a fatal error occurs (EXPORTED)
--
-- * fatal errors are IO monad errors and errors raised by `fatal' as well as
--   uncaught exceptions
--
-- * the base and generic state observed by the error handler is *in contrast
--   to `catch'* the state *before* the state transformer is applied
--
fatalsHandledBy :: PreCST e s a -> (IOError -> PreCST e s a) -> PreCST e s a
fatalsHandledBy m h  = CST $ StateTrans.fatalsHandledBy m' h'
		       where
		         m' = unpackCST m
			 h' = unpackCST . h

-- mutable variables
-- -----------------

-- lifted mutable variable functions (EXPORTED)
--

newMV :: a -> PreCST e s (StateTrans.MVar a)
newMV  = CST . StateTrans.newMV

readMV :: StateTrans.MVar a -> PreCST e s a
readMV  = CST . StateTrans.readMV

assignMV     :: StateTrans.MVar a -> a -> PreCST e s ()
assignMV m a  = CST $ StateTrans.assignMV m a

{- not used in c2hs:
newMA     :: Ix i => (i, i) -> a -> PreCST e s (StateTrans.MArr i a)
newMA b a  = CST $ StateTrans.newMA b a

readMA      :: Ix i => StateTrans.MArr i a -> i -> PreCST e s a
readMA  m i  = CST $ StateTrans.readMA m i

writeMA       :: Ix i => StateTrans.MArr i a -> i -> a -> PreCST e s ()
writeMA m i a  = CST $ StateTrans.writeMA m i a
 -}


-- read identification
-- -------------------

-- read identification information (EXPORT)
--
getId :: PreCST e s (String, String, String)
getId  = CST $ 
         readBase (idBS)


-- manipulating the error state
-- ----------------------------

-- the lowest level of errors is `WarningErr', but it is meaningless as long as
-- the the list of errors is empty
--
initialErrorState :: ErrorState
initialErrorState  = ErrorState WarningErr 0 []

-- raise an error (EXPORTED)
--
-- * a fatal error is reported immediately; see `raiseFatal'
--
raise     :: Error -> PreCST e s ()
raise err  = case errorLvl err of
	       WarningErr  -> raise0 err
	       ErrorErr    -> raise0 err
	       FatalErr    -> raiseFatal0 "Generic fatal error." err

-- raise a warning (see `raiseErr') (EXPORTED)
--
raiseWarning         :: Position -> [String] -> PreCST e s ()
raiseWarning pos msg  = raise0 (makeError WarningErr pos msg)

-- raise an error (see `raiseErr') (EXPORTED)
--
raiseError         :: Position -> [String] -> PreCST e s ()
raiseError pos msg  = raise0 (makeError ErrorErr pos msg)

-- raise a fatal compilation error (EXPORTED)
--
-- * the error is together with the up-to-now accumulated errors are reported
--   as part of the error message of the fatal error exception
--
-- * the current thread of control is discarded and control is passed to the
--   innermost handler for fatal errors
--
-- * the first argument must contain a short description of the error, while
--   the second and third argument are like the two arguments to `raise'
--
raiseFatal                :: String -> Position -> [String] -> PreCST e s a
raiseFatal short pos long  = raiseFatal0 short (makeError FatalErr pos long)

-- raise a fatal error; internal version that gets an abstract error
--
raiseFatal0           :: String -> Error -> PreCST e s a
raiseFatal0 short err  = do
			   raise0 err
			   errmsgs <- showErrors
			   fatal (short ++ "\n\n" ++ errmsgs)

-- raise an error; internal version, doesn't check whether the error is fatal
--
-- * the error is entered into the compiler state and a fatal error is
--   triggered if the `errorLimit' is reached
--
raise0     :: Error -> PreCST e s ()
raise0 err  = do
	        noOfErrs <- CST $ transBase doRaise
		when (noOfErrs >= errorLimit) $ do
		  errmsgs <- showErrors
		  fatal ("Error limit of " ++ show errorLimit 
			 ++ " errors has been reached.\n" ++ errmsgs)
  where
    doRaise    :: BaseState e -> (BaseState e, Int)
    doRaise bs  = let
		    lvl			       = errorLvl err
		    ErrorState wlvl no errs    = errorsBS bs
		    wlvl'		       = max wlvl lvl
		    no'			       = no + if lvl > WarningErr
						      then 1 else 0
		    errs'		       = err : errs
		  in
		    (bs {errorsBS = (ErrorState wlvl' no' errs')}, no')

-- yield a string containing the collected error messages (EXPORTED)
--
--  * the error state is reset in this process 
--
showErrors :: PreCST e s String
showErrors  = CST $ do
	        ErrorState wlvl no errs <- transBase extractErrs
		return $ foldr (.) id (map showString (errsToStrs errs)) ""
	      where
		extractErrs    :: BaseState e -> (BaseState e, ErrorState)
		extractErrs bs  = (bs {errorsBS = initialErrorState}, 
				   errorsBS bs)

		errsToStrs      :: [Error] -> [String]
		errsToStrs errs  = (map showError . sort) errs

-- inquire if there was already an error of at least level `ErrorErr' raised
-- (EXPORTED)
--
errorsPresent :: PreCST e s Bool
errorsPresent  = CST $ do 
		   ErrorState wlvl no _ <- readBase errorsBS
		   return $ wlvl >= ErrorErr


-- manipulating the extra state
-- ----------------------------

-- apply a reader function to the extra state and yield the reader's result
-- (EXPORTED) 
--
readExtra    :: (e -> a) -> PreCST e s a
readExtra rf  = CST $ readBase (\bs ->
		        (rf . extraBS) bs
		      )

-- apply an update function to the extra state (EXPORTED)
--
updExtra    :: (e -> e) -> PreCST e s ()
updExtra uf  = CST $ transBase (\bs ->
		       let
			 es = extraBS bs
		       in 
		       (bs {extraBS = uf es}, ())
		     )


-- name supplies
-- -------------

-- Get a name supply out of the base state (EXPORTED)
--
getNameSupply :: PreCST e s NameSupply
getNameSupply  = CST $ transBase (\bs ->
		         let
			   supply : supplies = suppliesBS bs
			 in 
			 (bs {suppliesBS = supplies}, supply)
		       )
