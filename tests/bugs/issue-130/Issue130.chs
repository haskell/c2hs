module Main where

import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Storable

#include "issue130.h"

main :: IO ()
main = do
  print (myAdd 1 2)
  print =<< myAddIO 1 2

{#fun pure unsafe my_add as myAdd   {`CInt', `CInt', alloca- `CInt' peek* } -> `()'#}
{#fun      unsafe my_add as myAddIO {`CInt', `CInt', alloca- `CInt' peek* } -> `()'#}
