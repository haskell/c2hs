module Main where

import Foreign
import Foreign.C

#include "issue20.h"

{#default size_t `CSize'#}
{#fun foo {`Int'} -> `CSize'#}

main :: IO ()
main = do
  s1 <- foo 1
  s4 <- foo 4
  print $ s4 `div` s1
