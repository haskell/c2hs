-- Foo.chs
{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Issue97A (
      Foo
    , FooPtr
    ) where

import Foreign

#include "issue97.h"

data Foo
{#pointer *foo_t as FooPtr -> Foo #}
