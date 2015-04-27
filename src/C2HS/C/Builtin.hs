--  C->Haskell Compiler: C builtin information
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 February 01
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

module C2HS.C.Builtin (
  builtinTypeNames
) where

-- Language.C / compiler toolkit
import Language.C.Data.Position
import Language.C.Data.Ident
import Language.C.Syntax
import Language.C.Data

import C2HS.C.Attrs (CObj(BuiltinCO))

-- | predefined type names
--
builtinTypeNames :: [(Ident, CObj)]
builtinTypeNames  =
    [(va_list_ide, BuiltinCO $ Just ptrVoidDecl)]
    where
        va_list_ide :: Ident
        va_list_ide = builtinIdent "__builtin_va_list"

        ptrVoidDecl :: CDecl
        ptrVoidDecl =
            CDecl [ CStorageSpec (CTypedef builtin)
                  , CTypeSpec (CVoidType builtin)
                  ]
                  [( Just $ CDeclr (Just va_list_ide)
                                   [CPtrDeclr [] builtin]
                                   Nothing
                                   []
                                   builtin
                   , Nothing
                   , Nothing
                   )]
                  builtin

        builtin :: NodeInfo
        builtin = mkNodeInfoOnlyPos builtinPos

