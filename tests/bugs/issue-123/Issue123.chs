module Main where

import Foreign

#include "issue123.h"

{#pointer *array_t as MyStruct#}

{#fun get_struct {`Int', `Int', `Int'} -> `MyStruct' return* #}

main :: IO ()
main = do
    myStruct <- get_struct 7 42 93
    p1 <- {#get array_t->p#} myStruct >>= peekArray 3
    print p1
    a1 <- {#get array_t->a#} myStruct >>= peekArray 3
    print a1
    cInts <- mallocArray 3
    pokeArray cInts [2, 4, 8]
    {#set array_t->p#} myStruct cInts
    p2 <- {#get array_t->p#} myStruct >>= peekArray 3
    print p2
    pokeArray cInts [3, 9, 27]
    {#set array_t->a#} myStruct cInts
    a2 <- {#get array_t->a#} myStruct >>= peekArray 3
    print a2
