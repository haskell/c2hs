module Main where

#include "issue46.h"

{#pointer *oid as Oid foreign newtype#}

{#fun func as ^ {+, `Int', `Float'} -> `Oid'#}
{#fun oid_a as ^ {`Oid'} -> `Int'#}
{#fun oid_b as ^ {`Oid'} -> `Float'#}

main :: IO ()
main = do
  obj <- func 1 2.5
  a <- oid_a obj
  b <- oid_a obj
  print (a, b)
