module Main where

#include "issue95.h"

main :: IO ()
main = do
  let s = {# sizeof foo #} :: Int
      a = {# alignof foo #} :: Int
  print s
  print a
