module Main where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#include "issue96.h"

{# pointer *foo_t as FooPtr newtype #}

get :: FooPtr -> IO CInt
get = {# get foo_t.x #}

set :: FooPtr -> CInt -> IO ()
set = {# set foo_t.x #}

call :: FooPtr -> IO ()
call = {# call simple_func #}

main :: IO ()
main = putStrLn "OK"
