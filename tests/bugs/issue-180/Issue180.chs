module Main where

#include "issue180.h"

marshalIn = undefined

{#fun pure test as test1
  { 'marshalIn'* `Int'&} -> `()' #}

main :: IO ()
main = do
  test
  print "OK"
