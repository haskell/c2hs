module Main where

import Foreign.C

#include "issue45.h"

main :: IO ()
main = foo 2
  where {#fun foo {`Int'} -> `()'#}
