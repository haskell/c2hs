module Main where

import Foreign
import Foreign.C

#include "issue51.h"

foo :: CInt -> CInt
#ifdef __GNUC__
foo = {#call pure fooGnu#}
#else
foo = {#call pure fooNonGnu#}
#endif

main = print $ foo (fromIntegral 0)
