--  Compiler Toolkit: compiler state management basics
--
--  Author : Manuel M. T. Chakravarty
--  Created: 7 November 97
--
--  Copyright (C) [1997..1999] Manuel M. T. Chakravarty
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
--  This module provides basic types and services used to realize the state
--  management of the compiler.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * The monad `PreCST' is an instance of `STB' where the base state is fixed.
--    However, the base state itself is parametrized by an extra state
--    component that can be instantiated by the compiler that uses the toolkit
--    (to store information like compiler switches) -- this is the reason for
--    adding the prefix `Pre'.
--
--  * The module exports the details of the `BaseState' etc as they have to be
--    know by `State'.  The latter ensures the necessary abstraction for
--    modules that do not belong to the state management.
--
--  * Due to this module, the state management modules can share internal
--    information about the data types hidden to the rest of the system.
--
--  * The following state components are maintained:
--
--    + errorsBS (type `ErrorState')    -- keeps track of raised errors
--    + namesBS (type `NameSupply')     -- provides unique names
--    + extraBS (generic type)          -- extra compiler-dependent state
--                                         information, e.g., for compiler
--                                         switches
--
--- TODO ----------------------------------------------------------------------
--

module Control.StateBase (PreCST(..), ErrorState(..), BaseState(..),
                  unpackCST, readCST, writeCST, transCST, liftIO)
where
import Control.StateTrans (STB, readGeneric, writeGeneric, transGeneric)
import qualified Control.StateTrans as StateTrans (liftIO)
import Data.Errors     (ErrorLevel(..), Error)
import Language.C.Data.Name

import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

-- state used in the whole compiler
-- --------------------------------

-- | form of the error state
--
-- * when no error was raised yet, the error level is the lowest possible one
--
data ErrorState = ErrorState ErrorLevel    -- worst error level that was raised
                             Int         -- number of errors (excl warnings)
                             [Error]     -- already raised errors

-- | base state
--
data BaseState e = BaseState {
                     errorsBS   :: ErrorState,
                     supplyBS   :: [Name],                    -- unique names
                     extraBS    :: e                          -- extra state
                 }

-- | the compiler state transformer
--

newtype PreCST e s a = CST (STB (BaseState e) s a)

instance Functor (PreCST e s) where
  fmap = liftM

instance Applicative (PreCST e s) where
  pure  = return
  (<*>) = ap

instance Monad (PreCST e s) where
  return = yield
  (>>=)  = (+>=)


-- | unwrapper coercion function
--
unpackCST   :: PreCST e s a -> STB (BaseState e) s a
unpackCST m  = let CST m' = m in m'


-- monad operations
-- ----------------

-- | the monad's unit
--
yield   :: a -> PreCST e s a
yield a  = CST $ return a

-- | the monad's bind
--
(+>=)   :: PreCST e s a -> (a -> PreCST e s b) -> PreCST e s b
m +>= k  = CST $ unpackCST m >>= (\a -> unpackCST (k a))


-- generic state manipulation
-- --------------------------

-- | given a reader function for the state, wrap it into an CST monad
--
readCST   :: (s -> a) -> PreCST e s a
readCST f  = CST $ readGeneric f

-- | given a new state, inject it into an CST monad
--
writeCST    :: s -> PreCST e s ()
writeCST s'  = CST $ writeGeneric s'

-- | given a transformer function for the state, wrap it into an CST monad
--
transCST   :: (s -> (s, a)) -> PreCST e s a
transCST f  = CST $ transGeneric f

-- interaction with the encapsulated 'IO' monad
-- --------------------------------------------

-- | lifts an 'IO' state transformer into 'CST'
--
liftIO   :: IO a -> PreCST e s a
liftIO m  = CST $ (StateTrans.liftIO m)
