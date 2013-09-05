module Main where

import Foreign
import Foreign.C

#include "issue54.h"

{#pointer *bar as Bar#}
{#pointer *foo as Foo#}

{#fun get_bar {`Int'} -> `Bar' return* #}
{#fun get_foo {`Int'} -> `Foo' return* #}

main :: IO ()
main = do
  bar <- get_bar 2
  c <- {#get bar->c#} bar
  d <- {#get bar->d#} bar
  print c
  print d
  foo <- get_foo 3
  a <- {#get foo->a#} foo
  b <- {#get foo->b#} foo
  print a
  print b
