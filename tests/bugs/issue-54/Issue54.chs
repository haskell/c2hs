module Main where

#include "issue54.h"

{#pointer *bar as Bar#}
{#pointer *foo as Foo#}

{#fun get_bar {`Int'} -> `Bar' return* #}
{#fun get_foo {`Int'} -> `Foo' return* #}

main :: IO ()
main = do
  bar <- get_bar 2
  c1 <- {#get bar->c#} bar
  d1 <- {#get bar->d#} bar
  print c1
  print d1
  c2 <- {#get bar.c#} bar
  d2 <- {#get bar.d#} bar
  print c2
  print d2
  foo <- get_foo 3
  a1 <- {#get struct foo->a#} foo
  b1 <- {#get struct foo->b#} foo
  print a1
  print b1
  a2 <- {#get struct foo.a#} foo
  b2 <- {#get struct foo.b#} foo
  print a2
  print b2
  a3 <- {#get foo->a#} foo
  b3 <- {#get foo->b#} foo
  print a3
  print b3
  a4 <- {#get foo.a#} foo
  b4 <- {#get foo.b#} foo
  print a4
  print b4
