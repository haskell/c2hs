{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Types where

#include "types.h"

data ExampleStruct
{# pointer *example_struct as ExampleStructPtr -> ExampleStruct #}
{# class ExampleStructClass ExampleStructPtr #}

data ChildStruct
{# pointer *child_struct as ChildStructPtr -> ChildStruct #}
{# class ExampleStructClass => ChildStructClass ChildStructPtr #}
