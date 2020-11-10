module Main where

import Foreign.Marshal.Utils(toBool)

#include "issue257.h"

main :: IO ()
main = do
  bools <- {#call make_bools#} (toBool True) (toBool False) (toBool True) (toBool False)
  a <- {#get bools->a#} bools
  b <- {#get bools->b#} bools
  c <- {#get bools->c#} bools
  d <- {#get bools->d#} bools
  putStrLn (show a)
  putStrLn (show b)
  putStrLn (show c)
  putStrLn (show d)
