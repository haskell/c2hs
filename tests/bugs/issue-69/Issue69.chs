module Main where

#include "issue69.h"

{#fun foo1 {`Int'} -> `()'#}
{# fun foo2 {`Int'} -> `()'#}

main :: IO ()
main = do
  foo1 2
  foo2 2
