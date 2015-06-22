module Main where

#include "issue45.h"

main :: IO ()
main = foo 2
  where {#fun foo {`Int'} -> `()'#}
