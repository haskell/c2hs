--  C->Haskell Compiler: custom wrapper generator
--
--  Author : Manuel M T Chakravarty
--  Created: 5 February 2003
--
--  Copyright (c) 2004 Manuel M T Chakravarty
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
--  This module implements the generation of a custom C file to wrap
--  functions requiring marshalling of bare C structs to pointers.
--

module C2HS.Gen.Wrapper (
  genWrapper
) where

-- Language.C / Compiler Toolkit
import Language.C.Data
import Language.C.Pretty
import Language.C.Syntax
import Data.DList (DList)
import qualified Data.DList as DL

-- C->Haskell
import C2HS.State (CST)

-- friends
import C2HS.CHS (CHSModule(..), CHSFrag(..), CHSHook(..),
                 CHSParm(..), CHSAPath(..), isParmWrapped)

import Debug.Trace

-- | Generate a custom C wrapper from a CHS binding module for
-- functions that require marshalling of bare C structs.
--
genWrapper :: CHSModule -> CST s [String]
genWrapper mod' = return $ gwModule mod'


-- | Generate the C wrapper for an entire .chs module.
--
gwModule :: CHSModule -> [String]
gwModule (CHSModule frags) = DL.toList $ DL.concat $ map gwFrag frags


-- | Process a single fragment.
--
gwFrag :: CHSFrag -> DList String
gwFrag (CHSHook (CHSFun _ _ _ _
                 (CHSRoot _ ide) oalias _ ips op _) _) =
  if any isParmWrapped ips
  then trace ("INPUT: " ++ show ips ++ "\nOUTPUT: " ++ show op) $
       DL.fromList ["WRAPPED:" ++ wrappername ++ "\n"]
  else DL.empty
  where wrappername = "__c2hs_wrapper__" ++ identToString ide
  -- let ideLexeme = identToString ide
  --     hsLexeme  = ideLexeme `maybe` identToString $ oalias
  --     vaIdent base idx = "__c2hs__vararg__" ++ base ++ "_" ++ show idx
  --     ides = map (vaIdent hsLexeme) [0..length varTypes - 1]
  --     defs = zipWith (\t i -> t ++ " " ++ i ++ ";\n") varTypes ides
  -- return (DL.fromList defs, Frag frag, frags)
gwFrag _ = DL.empty
