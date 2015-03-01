module Main where

import Foreign
import Foreign.C

#include "issue123.h"

{#pointer *array_t as MyStruct#}

{#fun get_struct {`Int', `Int', `Int'} -> `MyStruct' return* #}

main :: IO ()
main = do
    myStruct <- get_struct 7 42 93
    p <- {#get array_t->p#} myStruct >>= peekArray 3
    print p
    a <- {#get array_t->a#} myStruct >>= peekArray 3
    print a
    cInts <- mallocArray 3
    pokeArray cInts [2, 4, 8]
    {#set array_t->p#} myStruct cInts
    p <- {#get array_t->p#} myStruct >>= peekArray 3
    print p
    pokeArray cInts [3, 9, 27]
    {#set array_t->a#} myStruct cInts
    a <- {#get array_t->a#} myStruct >>= peekArray 3
    print a
