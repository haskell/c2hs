module Main where

import Foreign
import Foreign.C

#include "issue20.h"

{#fun foo {`Int'} -> `CSize'#}

main :: IO ()
main = do
  print $ foo 1
