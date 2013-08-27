module Main where

import Foreign
import Foreign.C

{#import Issue30Aux1#}
{#import Issue30Aux2#}

#include "issue30.h"

{#fun foo {`Int'} -> `Int'#}

main :: IO ()
main = do
  f <- foo 2
  f1 <- foo1 1
  f2 <- foo2 1
  print f
  print f1
  print f2
