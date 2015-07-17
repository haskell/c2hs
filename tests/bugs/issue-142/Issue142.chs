module Main where

#include "issue142.h"

{#fun fn1 as ^ {} -> `String' #}
{#fun fn2 as ^ {} -> `String' #}

main :: IO ()
main = do
  fn1 >>= putStrLn
  fn2 >>= putStrLn
