module Main where

#include "issue47.h"

{#fun foo {`Int'} -> `()'#}

main :: IO ()
main = foo 2
