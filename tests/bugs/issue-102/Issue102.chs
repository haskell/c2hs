module Main where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include <stdio.h>

{#fun variadic printf(int) as printi {`String', `Int'} -> `()'#}
{#fun variadic printf(int, int) as printi2 {`String', `Int', `Int'} -> `()'#}

main :: IO ()
main = do
  printi "TST 1: %d\n" 1234
  printi2 "TST 2: %d %d\n" 13 47
