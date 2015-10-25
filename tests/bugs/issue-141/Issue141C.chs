module Main where

#include "issue141.h"

main :: IO ()
main = do
  let f = {#get _p_Vec->fieldname#}
  print "OK"
