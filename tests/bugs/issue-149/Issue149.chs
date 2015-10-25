module Main where

#include "issue149.h"

{#fun unsafe test as ^ {} -> `()'#}

main :: IO ()
main = do
  test
  print "OK"
