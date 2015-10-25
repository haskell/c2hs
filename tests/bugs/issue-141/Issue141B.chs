module Main where

#include "issue141.h"

main :: IO ()
main = do
  print {#alignof _p_Vec#}
  print "OK"
