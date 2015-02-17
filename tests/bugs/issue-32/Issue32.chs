module Main where

import Data.Bits
import Foreign.C
import Foreign.Ptr
import Foreign.Storable

#include "issue32.h"

{#pointer *testStruct as TestStructPtr #}

main :: IO ()
main = do
  x <- {#call makeIt #}
  print =<< ({#get testStruct->a #} x)
  print =<< ({#get testStruct->b #} x)
  print =<< ({#get testStruct->c #} x)
