--  Compiler Toolkit: compiler state management
--
--  Author : Manuel M. T. Chakravarty
--  Created: 2 November 95
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

module Control.State (-- the PreCST monad
              --
              PreCST,                                      -- reexport ABSTRACT
              throwExc, fatal, catchExc, fatalsHandledBy,  -- reexport lifted
              readCST, writeCST, transCST, run, runCST,
              --
              -- more compiler I/O
              --
              liftIO,
              --
              -- error management
              --
              raise, raiseWarning, raiseError, raiseFatal, showErrors,
              errorsPresent,
              --
              -- state management helpers
              getNameSupply, setNameSupply,
              --
              -- extra state management
              --
              readExtra, updExtra)
where

import Control.Monad (when)
import Control.StateTrans  (readBase, transBase, runSTB)
import qualified Control.StateTrans as StateTrans (interleave, throwExc, fatal, catchExc, fatalsHandledBy)
import Control.StateBase   (PreCST(..), ErrorState(..), BaseState(..),
                    unpackCST, readCST, writeCST, transCST,
                    liftIO)
import qualified System.CIO as CIO
import Data.Errors      (Error, makeError)
import Language.C.Data.Name
import Language.C.Data.Position
import Language.C.Data.Error hiding (Error)


-- state used in the whole compiler
-- --------------------------------

-- | initialization
--
-- * it gets the version information and the initial extra state as arguments
--
initialBaseState   :: e -> BaseState e
initialBaseState es = BaseState {
                             supplyBS   = newNameSupply,
                             errorsBS   = initialErrorState,
                             extraBS    = es
                        }


-- executing state transformers
-- ----------------------------

-- | initiate a complete run of the ToolKit represented by a PreCST with a void
-- generic component (type '()')
--
-- * fatals errors are explicitly caught and reported (instead of letting them
--   through to the runtime system)
--
run       :: e -> PreCST e () a -> IO a
run es cst = runSTB m (initialBaseState es) ()
  where
    m = unpackCST (
          cst
          `fatalsHandledBy` \err ->
            CIO.putStr ("Uncaught fatal error: " ++ show err)   >>
            CIO.exitWith (CIO.ExitFailure 1)
        )

-- | run a PreCST in the context of another PreCST
--
-- the generic state of the enclosing PreCST is preserved while the
-- computation of the PreCST passed as an argument is interleaved in the
-- execution of the enclosing one
--
runCST     :: PreCST e s a -> s -> PreCST e s' a
runCST m s  = CST $ StateTrans.interleave (unpackCST m) s


-- exception handling
-- ------------------

-- | throw an exception with the given tag and message
--
throwExc       :: String -> String -> PreCST e s a
throwExc s1 s2  = CST $ StateTrans.throwExc s1 s2

-- | raise a fatal user-defined error
--
-- * such an error my be caught and handled using 'fatalsHandeledBy'
--
fatal :: String -> PreCST e s a
fatal  = CST . StateTrans.fatal

-- | the given state transformer is executed and exceptions with the given tag
-- are caught using the provided handler, which expects to get the exception
-- message
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
fatalsHandledBy :: PreCST e s a -> (IOError -> PreCST e s a) -> PreCST e s a
fatalsHandledBy m h  = CST $ StateTrans.fatalsHandledBy m' h'
                       where
                         m' = unpackCST m
                         h' = unpackCST . h


-- manipulating the error state
-- ----------------------------

-- | the lowest level of errors is 'LevelWarn', but it is meaningless as long as
-- the the list of errors is empty
--
initialErrorState :: ErrorState
initialErrorState  = ErrorState LevelWarn 0 []

-- | raise an error
--
-- * a fatal error is reported immediately; see 'raiseFatal'
--
raise     :: Error -> PreCST e s ()
raise err  = case errorLevel err of
               LevelWarn  -> raise0 err
               LevelError    -> raise0 err
               LevelFatal    -> raiseFatal0 "Generic fatal error." err

-- | raise a warning (see 'raiseErr')
--
raiseWarning         :: Position -> [String] -> PreCST e s ()
raiseWarning pos msg  = raise0 (makeError LevelWarn pos msg)

-- | raise an error (see 'raiseErr')
--
raiseError         :: Position -> [String] -> PreCST e s ()
raiseError pos msg  = raise0 (makeError LevelError pos msg)

-- | raise a fatal compilation error
--
-- * the error is together with the up-to-now accumulated errors are reported
--   as part of the error message of the fatal error exception
--
-- * the current thread of control is discarded and control is passed to the
--   innermost handler for fatal errors
--
-- * the first argument must contain a short description of the error, while
--   the second and third argument are like the two arguments to 'raise'
--
raiseFatal                :: String -> Position -> [String] -> PreCST e s a
raiseFatal short pos long  = raiseFatal0 short (makeError LevelFatal pos long)

-- | raise a fatal error; internal version that gets an abstract error
--
raiseFatal0           :: String -> Error -> PreCST e s a
raiseFatal0 short err  = do
                           raise0 err
                           errmsgs <- showErrors
                           fatal (short ++ "\n\n" ++ errmsgs)

-- | raise an error; internal version, doesn't check whether the error is fatal
--
-- * the error is entered into the compiler state and a fatal error is
--   triggered if the 'errorLimit' is reached
--
raise0     :: Error -> PreCST e s ()
raise0 err  = do
                noOfErrs <- CST $ transBase doRaise
                when (noOfErrs >= errorLimit) $ do
                  errmsgs <- showErrors
                  fatal ("Error limit of " ++ show errorLimit
                         ++ " errors has been reached.\n" ++ errmsgs)
  where
    errorLimit = 20

    doRaise    :: BaseState e -> (BaseState e, Int)
    doRaise bs  = let
                    lvl                        = errorLevel err
                    ErrorState wlvl no errs    = errorsBS bs
                    wlvl'                      = max wlvl lvl
                    no'                        = no + if lvl > LevelWarn
                                                      then 1 else 0
                    errs'                      = err : errs
                  in
                    (bs {errorsBS = (ErrorState wlvl' no' errs')}, no')

-- | yield a string containing the collected error messages
--
--  * the error state is reset in this process
--
showErrors :: PreCST e s String
showErrors  = CST $ do
                ErrorState _ _ errs <- transBase extractErrs
                return $ concatMap (showErrorInfo "" . errorInfo) errs
                --FIXME: should be using show here ^^, but Show instance
                --       for CError from language-c is weird
              where
                extractErrs    :: BaseState e -> (BaseState e, ErrorState)
                extractErrs bs  = (bs {errorsBS = initialErrorState},
                                   errorsBS bs)

-- | inquire if there was already an error of at least level 'LevelError' raised
--
errorsPresent :: PreCST e s Bool
errorsPresent  = CST $ do
                   ErrorState wlvl _ _ <- readBase errorsBS
                   return $ wlvl >= LevelError

-- helpers for manipulating state
-- ----------------------------

-- | get a name supply
getNameSupply :: PreCST e s [Name]
getNameSupply = CST $ readBase supplyBS

-- | update the name supply
setNameSupply :: [Name] -> PreCST e s ()
setNameSupply ns = CST $ transBase $ \st -> (st { supplyBS = ns }, ())

-- manipulating the extra state
-- ----------------------------

-- | apply a reader function to the extra state and yield the reader's result
--
readExtra    :: (e -> a) -> PreCST e s a
readExtra rf  = CST $ readBase (\bs ->
                        (rf . extraBS) bs
                      )

-- | apply an update function to the extra state
--
updExtra    :: (e -> e) -> PreCST e s ()
updExtra uf  = CST $ transBase (\bs ->
                       let
                         es = extraBS bs
                       in
                       (bs {extraBS = uf es}, ())
                     )
