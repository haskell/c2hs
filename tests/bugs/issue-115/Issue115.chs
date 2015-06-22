module Main where

import Foreign.Marshal.Array

#include "issue115.h"

{#pointer *array_t as MyStruct#}

{#fun get_struct {`Int', `Int', `Int'} -> `MyStruct' return* #}

main :: IO ()
main = do
    myStruct <- get_struct 7 42 93
    p <- {#get array_t->p#} myStruct >>= peekArray 3
    print p
    -- The following line produces a segmentation fault
    a <- {#get array_t->a#} myStruct >>= peekArray 3
    print a
