module Main where

#include "issue257.h"

import Foreign.Ptr

{#fun make_bools as make_bools {`Bool',`Bool',`Bool',`Bool'} -> `Ptr ()' #}

main :: IO ()
main = do
  bools <- make_bools True False True False
  a <- {#get bools->a#} bools
  b <- {#get bools->b#} bools
  c <- {#get bools->c#} bools
  d <- {#get bools->d#} bools
  putStrLn (show a)
  putStrLn (show b)
  putStrLn (show c)
  putStrLn (show d)
