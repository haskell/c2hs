--  The Compiler Toolkit: configuration switches
--
--  Author : Manuel M. T. Chakravarty
--  Created: 3 October 95
--
--  Version $Revision: 1.3 $ from $Date: 1999/09/27 08:44:42 $
--
--  Copyright (c) [1995...1999] Manuel M. T. Chakravarty
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
--  This modules is used to configure the toolkit.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * Must not import any other module.
--
--- TODO ----------------------------------------------------------------------
--

module Config (-- limits
	       --
	       errorLimit,
	       --
	       -- debuging
	       --
	       assertEnabled)
where

-- compilation aborts with a fatal error, when the given number of errors
-- has been raised (warnings do not count)
--
errorLimit :: Int
errorLimit  = 20

-- specifies whether the internal consistency checks with `assert' should be
-- made
--
assertEnabled :: Bool
assertEnabled  = True
