--  C -> Haskell Compiler: C2HS's state
--
--  Author : Manuel M. T. Chakravarty
--  Created: 6 March 1999
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
--  This module instantiates the Compiler Toolkit's extra state with C2HS's
--  uncommon state information that should be stored in the Toolkit's base
--  state.
--
--  This modules re-exports everything provided by `State', and thus, should be
--  used as the single reference to state related functionality within C2HS.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  State components:
--
--    - compiler switches
--
--- TODO ----------------------------------------------------------------------
--

module C2HS.State (-- re-exports all of `State'
                  --
                  module Control.State,
                  --
                  -- instantiation of `PreCST' with C2HS's extra state
                  --
                  CST, runC2HS,
                  --
                  -- switches
                  --
                  SwitchBoard(..), Traces(..), setTraces, traceSet,
                  putTraceStr, setSwitch, getSwitch)
where

import Control.Monad (when)
import System.IO (stderr)

import Control.State
import qualified System.CIO as CIO

import C2HS.Switches (SwitchBoard(..), Traces(..),
                 initialSwitchBoard)


-- instantiation of the extra state
-- --------------------------------

-- | the extra state consists of the `SwitchBoard'
--
type CST s a = PreCST SwitchBoard s a

-- | execution of c2hs starts with the initial `SwitchBoard'
--
runC2HS :: CST () a -> IO a
runC2HS  = run initialSwitchBoard

-- switch management
-- -----------------

-- | set traces according to the given transformation function
--
setTraces   :: (Traces -> Traces) -> CST s ()
setTraces t  = updExtra (\es -> es {tracesSB = t (tracesSB es)})

-- | inquire the status a trace using the given inquiry function
--
traceSet   :: (Traces -> Bool) -> CST s Bool
traceSet t  = readExtra (t . tracesSB)

-- | output the given string to `stderr' when the trace determined by the inquiry
-- function is activated
--
putTraceStr       :: (Traces -> Bool) -> String -> CST s ()
putTraceStr t msg  = do
                       set <- traceSet t
                       when set $
                         CIO.hPutStr stderr msg

-- | set a switch value
--
setSwitch :: (SwitchBoard -> SwitchBoard) -> CST s ()
setSwitch  = updExtra

-- | get a switch values
--
getSwitch :: (SwitchBoard -> a) -> CST s a
getSwitch  = readExtra
