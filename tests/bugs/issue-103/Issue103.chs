module Main where

import Foreign.C.Types

#include "issue103.h"
{#import Issue103A#}

{#fun unsafe test_func as ^ { `TestEnum' } -> `()' #}

main :: IO ()
main = do
  testFunc E1
  testFunc E2
  testFunc E3
