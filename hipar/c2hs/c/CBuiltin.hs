--  C->Haskell Compiler: C builtin information
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 February 01
--
--  Version $Revision: 1.1 $
--
--  Copyright (c) 2001 Manuel M. T. Chakravarty
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
--  This module provides information about builtin entities.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  Currently, only builtin type names are supported.  The only builtin type
--  name is `__builtin_va_list', which is a builtin of GNU C.
--
--- TODO ----------------------------------------------------------------------
--

module CBuiltin (
  builtinTypeNames
) where

import Common (Position, Pos(..), builtinPos)
import Idents (Ident, onlyPosIdent)

import CAttrs (CObj(BuiltinCO))


-- predefined type names
--
builtinTypeNames :: [(Ident, CObj)]
builtinTypeNames  = [(onlyPosIdent builtinPos "__builtin_va_list", BuiltinCO)]
