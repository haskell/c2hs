module Main where

import Foreign.C

#include "issue47.h"

{#fun foo {`Int'} -> `()'#}

main :: IO ()
main = foo 2
