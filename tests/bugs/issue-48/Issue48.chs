module Main where

import Foreign.C.Types

#include "issue48.h"

{#typedef int64_t CLong#}
{#default out `Int' [int64_t] fromIntegral#}
{#default in `Int' [int64_t] fromIntegral#}
{#fun foo {`Int'} -> `Int'#}

main :: IO ()
main = do
  foo 1 >>= print
  foo 4 >>= print
