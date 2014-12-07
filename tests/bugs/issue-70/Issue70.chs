{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Foo where

#include "issue70.h"

class Flux a where
  data FluxCode a
  gigawattsNeeded :: a -> Double
  gigawattsNeeded _ = 1.21

data Capacitor = Capacitor Int

instance Flux Capacitor where
  -- associated data type decl
  data FluxCode Capacitor = Bar | Baz | Qux | Xyzzy

-- Note: must be able to define longer names here, I've used single quotes.
-- underscoreToCase still works, it aliases the C identifiers for the instance.
-- XYZZY_THUD is manually aliased.
-- nocode suppresses emitting a data declaration.
{# enum Foo as 'FluxCode Capacitor' nocode { underscoreToCase,
                                             XYZZY_THUD as Xyzzy } #}
