--  Compiler Toolkit: general purpose attribute management
--
--  Author : Manuel M. T. Chakravarty
--  Created: 14 February 95
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
--  This module provides an abstract notion of attributes (in the sense of
--  compiler construction). The collection of attributes that is attached to a
--  single node of the structure tree is referenced via an attributes
--  identifier. This is basically a reference into so-called attribute tables,
--  which manage attributes of one type and may use different representations.
--  There is also a position attribute managed via the attribute identifier
--  without needing a further table (it is already fixed on construction of
--  the structure tree).
--
--  The `Attributed' class is based on a suggestion from Roman Lechtchinsky.
--
--- DOCU ----------------------------------------------------------------------
--
--  language: Haskell 98
--
--  * Attribute identifiers are generated during parsing and whenever new
--    structure tree elements, possibly due to transformations, are generated.
--
--  * New attributes can be added by simply providing a new attribute table
--    indexed by the attribute identifiers. Thus, adding or discarding an
--    attribute does not involve any change in the structure tree.
--
--  * Consecutive sequences of names are used as attribute identifiers to
--    facilitate the use of arrays for attributes that are fixed; speeds up
--    read access. (See also TODO.)
--
--  * Each attribute table can simultaneously provide melted (updatable) and
--    frozen (non-updatable) attributes. It also allows to dynamically grow the
--    table, i.e., cover a wider range of attribute identifiers.
--
--  * There is a variant merely providing a position, which is used for
--    internal identifiers and such.
--
--  * `StdAttr' provides standard undefined and don't care variants for
--    attribute values.
--
--- TODO ----------------------------------------------------------------------
--
--  * When there are sparse attribute tables that we want to freeze (and they
--    will occur sooner or later), then introduce a third variant of tables
--    realized via hash table---depending on the type of attribute table, we
--    may even allow them to be soft.
--
--    NOTE: Currently, if assertions are switched on, on freezing a table, its
--          density is calculate and, if it is below 33%, an internal error is
--          raised (only if there are more than 1000 entries in the table).
--
--  * check whether it would increase the performance significantly if we use
--    a mixed finite map/array representation for soft tables (all attributes
--    defined before the last `soften' could be held in the array, changing
--    an attribute just means to update it in the FM; i.e., the FM entries take
--    precedence over the array entries)
--

module Data.Attributes (-- attribute management
                   --
                   NodeInfo, newAttrsOnlyPos, newAttrs,
                   Attributed(attrsOf), eqOfAttrsOf, posOfAttrsOf,
                   --
                   -- attributes and attribute tables
                   --
                   Attr(undef, isUndef, dontCare, isDontCare),
                   AttrTable, newAttrTable, getAttr, setAttr, updAttr,
                   copyAttr, freezeAttrTable, softenAttrTable,
                   StdAttr(..), getStdAttr, getStdAttrDft, isDontCareStdAttr,
                   isUndefStdAttr, setStdAttr, updStdAttr,
                   getGenAttr, setGenAttr, updGenAttr)
where

import Data.Array
import Control.Exception (assert)
import qualified Data.IntMap as NameMap (fromList, insert, findWithDefault, empty, assocs)
import Data.IntMap (IntMap)
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Data.Name (Name(Name, nameId))
import Data.Errors     (interr)

type NameMap = IntMap

-- attribute management data structures and operations
-- ---------------------------------------------------

-- a class for convenient access to the attributes of an attributed object
--
--
class Attributed a where
  attrsOf :: a -> NodeInfo

-- equality induced by attribution
--
eqOfAttrsOf           :: Attributed a => a -> a -> Bool
eqOfAttrsOf obj1 obj2  = (attrsOf obj1) == (attrsOf obj2)

-- position induced by attribution
--
posOfAttrsOf :: Attributed a => a -> Position
posOfAttrsOf  = posOf . attrsOf


-- attribute identifier creation
-- -----------------------------

-- Given only a source position, create a new attribute identifier
--
newAttrsOnlyPos     :: Position -> NodeInfo
newAttrsOnlyPos    = mkNodeInfoOnlyPos

-- Given a source position and a unique name, create a new attribute
-- identifier
--
newAttrs          :: Position -> Name -> NodeInfo
newAttrs           = mkNodeInfo

-- attribute tables and operations on them
-- ---------------------------------------

-- | the type class 'Attr' determines which types may be used as attributes
--
--
-- * such types have to provide values representing an undefined and a don't
--   care state, together with two functions to test for these values
--
-- * an attribute in an attribute table is initially set to 'undef' (before
--   some value is assigned to it)
--
-- * an attribute with value 'dontCare' participated in an already detected
--   error, it's value may not be used for further computations in order to
--   avoid error avalanches
--
class Attr a where
  undef      :: a
  isUndef    :: a -> Bool
  dontCare   :: a
  isDontCare :: a -> Bool
  undef       = interr "Attributes: Undefined `undef' method in `Attr' class!"
  isUndef     = interr "Attributes: Undefined `isUndef' method in `Attr' \
                       \class!"
  dontCare    = interr "Attributes: Undefined `dontCare' method in `Attr' \
                       \class!"
  isDontCare  = interr "Attributes: Undefined `isDontCare' method in `Attr' \
                       \class!"

-- | attribute tables map attribute identifiers to attribute values
--
-- * the attributes within a table can be soft or frozen, the former may by be
--   updated, but the latter can not be changed
--
-- * the attributes in a frozen table are stored in an array for fast
--   lookup; consequently, the attribute identifiers must be *dense*
--
-- * the table description string is used to emit better error messages (for
--   internal errors)
--
data Attr a =>
     AttrTable a = -- for all attribute identifiers not contained in the
                   -- finite map the value is 'undef'
                   --
                   SoftTable (NameMap a)   -- updated attr.s
                             String               -- desc of the table

                   -- the array contains 'undef' attributes for the undefined
                   -- attributes; for all attribute identifiers outside the
                   -- bounds, the value is also 'undef';
                   --
                 | FrozenTable (Array Name a)     -- attribute values
                               String             -- desc of the table

instance (Attr a, Show a) => Show (AttrTable a) where
  show     (SoftTable mp descr) = -- freeze is disabled
    "AttrTable "++ descr ++ " { " ++ (unwords . map show) (NameMap.assocs mp) ++ " }"
  show tbl@(FrozenTable _ _) = show (softenAttrTable tbl)

nameMapToList :: NameMap a -> [(Name, a)]
nameMapToList = map (\(k,v) -> (Name k, v)) . NameMap.assocs
nameMapFromList :: [(Name, a)] -> NameMap a
nameMapFromList = NameMap.fromList . map (\(k,v) -> (nameId k, v))

-- | create an attribute table, where all attributes are 'undef'
--
-- the description string is used to identify the table in error messages
-- (internal errors); a table is initially soft
--
newAttrTable      :: Attr a => String -> AttrTable a
newAttrTable desc  = SoftTable NameMap.empty desc

-- | get the value of an attribute from the given attribute table
--
getAttr                      :: Attr a => AttrTable a -> NodeInfo -> a
getAttr at node =
    case nameOfNode node of
        Nothing  -> onlyPosErr "getAttr" at (posOfNode node)
        Just aid ->
          case at of
            (SoftTable   fm  _) -> NameMap.findWithDefault undef (nameId aid) fm
            (FrozenTable arr _) -> let (lbd, ubd) = bounds arr
                                   in
                                   if (aid < lbd || aid > ubd) then undef else arr!aid

-- | set the value of an, up to now, undefined attribute from the given
-- attribute table
--
setAttr :: Attr a => AttrTable a -> NodeInfo -> a -> AttrTable a
setAttr at node av =
    case nameOfNode node of
        Nothing  -> onlyPosErr "setAttr" at (posOfNode node)
        Just aid ->
          case at of
            (SoftTable fm desc) -> assert (isUndef (NameMap.findWithDefault undef (nameId aid) fm)) $
                                     SoftTable (NameMap.insert (nameId aid) av fm) desc
            (FrozenTable _arr _) -> interr frozenErr
          where
            frozenErr     = "Attributes.setAttr: Tried to write frozen attribute in\n"
                            ++ errLoc at (posOfNode node)

-- | update the value of an attribute from the given attribute table
--
updAttr :: Attr a => AttrTable a -> NodeInfo -> a -> AttrTable a
updAttr at node av =
    case nameOfNode node of
        Nothing  -> onlyPosErr "updAttr" at (posOfNode node)
        Just aid ->
          case at of
            (SoftTable   fm  desc) -> SoftTable (NameMap.insert (nameId aid) av fm) desc
            (FrozenTable _arr _)    -> interr $ "Attributes.updAttr: Tried to\
                                               \ update frozen attribute in\n"
                                               ++ errLoc at (posOfNode node)

-- | copy the value of an attribute to another one
--
-- * undefined attributes are not copied, to avoid filling the table
--
copyAttr :: Attr a => AttrTable a -> NodeInfo -> NodeInfo -> AttrTable a
copyAttr at ats ats'
  | isUndef av = assert (isUndef (getAttr at ats'))
                   at
  | otherwise  = 
    updAttr at ats' av
  where
    av = getAttr at ats

-- | auxiliary functions for error messages
--
onlyPosErr                :: Attr a => String -> AttrTable a -> Position -> b
onlyPosErr fctName at pos  =
  interr $ "Attributes." ++ fctName ++ ": No attribute identifier in\n"
           ++ errLoc at pos
--
errLoc        :: Attr a => AttrTable a -> Position -> String
errLoc at pos  = "  table `" ++ tableDesc at ++ "' for construct at\n\
                 \  position " ++ show pos ++ "!"
  where
    tableDesc (SoftTable   _ desc) = desc
    tableDesc (FrozenTable _ desc) = desc

-- | freeze a soft table; afterwards no more changes are possible until the
-- table is softened again
--
freezeAttrTable                        :: Attr a => AttrTable a -> AttrTable a
freezeAttrTable (SoftTable   fm  desc) =
  let contents = nameMapToList fm
      keys     = map fst contents
      lbd      = minimum keys
      ubd      = maximum keys
  in
  assert (length keys < 1000 || (length . range) (lbd, ubd) > 3 * length keys)
  (FrozenTable (array (lbd, ubd) contents) desc)
freezeAttrTable (FrozenTable _   desc)  =
  interr ("Attributes.freezeAttrTable: Attempt to freeze the already frozen\n\
          \  table `" ++ desc ++ "'!")

-- | soften a frozen table; afterwards changes are possible until the
-- table is frozen again
--
softenAttrTable                        :: Attr a => AttrTable a -> AttrTable a
softenAttrTable (SoftTable   _fm desc)  =
  interr ("Attributes.softenAttrTable: Attempt to soften the already \
          \softened\n  table `" ++ desc ++ "'!")
softenAttrTable (FrozenTable arr desc)  =
  SoftTable (nameMapFromList . assocs $ arr) desc


-- standard attributes
-- -------------------

-- | standard attribute variants
--
data StdAttr a = UndefStdAttr
               | DontCareStdAttr
               | JustStdAttr a

instance Attr (StdAttr a) where
  undef = UndefStdAttr

  isUndef UndefStdAttr = True
  isUndef _            = False

  dontCare = DontCareStdAttr

  isDontCare DontCareStdAttr = True
  isDontCare _               = False

-- | get an attribute value from a standard attribute table
--
-- * if the attribute can be "don't care", this should be checked before
--   calling this function (using 'isDontCareStdAttr')
--
getStdAttr         :: AttrTable (StdAttr a) -> NodeInfo -> a
getStdAttr atab at  = getStdAttrDft atab at err
  where
    err = interr $ "Attributes.getStdAttr: Don't care in\n"
                   ++ errLoc atab (posOf at)

-- | get an attribute value from a standard attribute table, where a default is
-- substituted if the table is don't care
--
getStdAttrDft             :: AttrTable (StdAttr a) -> NodeInfo -> a -> a
getStdAttrDft atab at dft  =
  case getAttr atab at of
    DontCareStdAttr -> dft
    JustStdAttr av  -> av
    UndefStdAttr    -> interr $ "Attributes.getStdAttrDft: Undefined in\n"
                                ++ errLoc atab (posOf at)

-- | check if the attribue value is marked as "don't care"
--
isDontCareStdAttr         :: AttrTable (StdAttr a) -> NodeInfo -> Bool
isDontCareStdAttr atab at  = isDontCare (getAttr atab at)

-- | check if the attribue value is still undefined
--
-- * we also regard "don't care" attributes as undefined
--
isUndefStdAttr         :: AttrTable (StdAttr a) -> NodeInfo -> Bool
isUndefStdAttr atab at  = isUndef (getAttr atab at)

-- | set an attribute value in a standard attribute table
--
setStdAttr :: AttrTable (StdAttr a) -> NodeInfo -> a -> AttrTable (StdAttr a)
setStdAttr atab at av = setAttr atab at (JustStdAttr av)

-- | update an attribute value in a standard attribute table
--
updStdAttr :: AttrTable (StdAttr a) -> NodeInfo -> a -> AttrTable (StdAttr a)
updStdAttr atab at av = updAttr atab at (JustStdAttr av)


-- generic attribute table access
-- ------------------------------

getGenAttr         :: (Attr a, Attributed obj) => AttrTable a -> obj -> a
getGenAttr atab at  = getAttr atab (attrsOf at)

setGenAttr            :: (Attr a, Attributed obj)
                      => AttrTable a -> obj -> a -> AttrTable a
setGenAttr atab at av  = setAttr atab (attrsOf at) av

updGenAttr            :: (Attr a, Attributed obj)
                      => AttrTable a -> obj -> a -> AttrTable a
updGenAttr atab at av  = updAttr atab (attrsOf at) av
