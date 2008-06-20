--  Compiler Toolkit: name space management
--
--  Author : Manuel M. T. Chakravarty
--  Created: 12 November 95
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
--  This module manages name spaces.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * A name space associates identifiers with their definition.
--
--  * Each name space is organized in a hierarchical way using the notion of
--    ranges. A name space, at any moment, always has a global range and may
--    have several local ranges. Definitions in inner ranges hide definitions
--    of the same identifiert in outer ranges.
--
--- TODO ----------------------------------------------------------------------
--
--  * evaluate the performance gain that a hashtable would bring
--

module Data.NameSpaces (NameSpace, nameSpace, defGlobal, enterNewRange, leaveRange,
                   defLocal, find, nameSpaceToList)
where

import qualified Data.Map as Map (empty, insert, lookup, toList)
import Data.Map   (Map)
import Data.Idents     (Ident)
import Data.Errors     (interr)


-- name space (EXPORTED ABSTRACT)
--
-- * the definitions in the global ranges are stored in a finite map, because
--   they tend to be a lot and are normally not updated after the global range
--   is constructed
--
-- * the definitions of the local ranges are stored in a single list, usually
--   they are not very many and the definitions entered last are the most
--   frequently accessed ones; the list structure naturally hides older
--   definitions, i.e., definitions from outer ranges; adding new definitions
--   is done in time proportinal to the current size of the range; removing a
--   range is done in constant time (and the definitions of a range can be
--   returned as a result of leaving the range); lookup is proportional to the
--   number of definitions in the local ranges and the logarithm of the number
--   of definitions in the global range---i.e., efficiency relies on a
--   relatively low number of local definitions together with frequent lookup
--   of the most recently defined local identifiers
--
data NameSpace a = NameSpace (Map Ident a)  -- defs in global range
                             [[(Ident, a)]]       -- stack of local ranges

-- create a name space (EXPORTED)
--
nameSpace :: NameSpace a
nameSpace  = NameSpace Map.empty []

-- add global definition (EXPORTED)
--
-- * returns the modfied name space
--
-- * if the identfier is already declared, the resulting name space contains
--   the new binding and the second component of the result contains the
--   definition declared previosuly (which is henceforth not contained in the
--   name space anymore)
--
defGlobal :: NameSpace a -> Ident -> a -> (NameSpace a, Maybe a)
defGlobal (NameSpace gs lss) id def  = (NameSpace (Map.insert id def gs) lss,
                                        Map.lookup id gs)

-- add new range (EXPORTED)
--
enterNewRange                    :: NameSpace a -> NameSpace a
enterNewRange (NameSpace gs lss)  = NameSpace gs ([]:lss)

-- pop topmost range and return its definitions (EXPORTED)
--
leaveRange :: NameSpace a -> (NameSpace a, [(Ident, a)])
leaveRange (NameSpace gs [])        = interr "NameSpaces.leaveRange: \
                                             \No local range!"
leaveRange (NameSpace gs (ls:lss))  = (NameSpace gs lss, ls)

-- add local definition (EXPORTED)
--
-- * returns the modfied name space
--
-- * if there is no local range, the definition is entered globally
--
-- * if the identfier is already declared, the resulting name space contains
--   the new binding and the second component of the result contains the
--   definition declared previosuly (which is henceforth not contained in the
--   name space anymore)
--
defLocal :: NameSpace a -> Ident -> a -> (NameSpace a, Maybe a)
defLocal ns@(NameSpace gs []      ) id def = defGlobal ns id def
defLocal (NameSpace    gs (ls:lss)) id def =
  (NameSpace gs (((id, def):ls):lss),
   lookup ls)
  where
    lookup []                          = Nothing
    lookup ((id', def):ls) | id == id' = Just def
                           | otherwise = lookup ls

-- search for a definition (EXPORTED)
--
-- * the definition from the innermost range is returned, if any
--
find                       :: NameSpace a -> Ident -> Maybe a
find (NameSpace gs lss) id  = case (lookup lss) of
                                Nothing  -> Map.lookup id gs
                                Just def -> Just def
                              where
                                lookup []       = Nothing
                                lookup (ls:lss) = case (lookup' ls) of
                                                    Nothing  -> lookup lss
                                                    Just def -> Just def

                                lookup' []              = Nothing
                                lookup' ((id', def):ls)
                                        | id' == id     = Just def
                                        | otherwise     = lookup' ls

-- dump a name space into a list (EXPORTED)
--
-- * local ranges are concatenated
--
nameSpaceToList                    :: NameSpace a -> [(Ident, a)]
nameSpaceToList (NameSpace gs lss)  = Map.toList gs ++ concat lss
