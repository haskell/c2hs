module Main where

import Foreign.C
import System.IO.Unsafe (unsafePerformIO)

{#nonGNU#}
#include "issue51.h"

foo :: CInt -> CInt
#ifdef __GNUC__
foo = {#call pure fooGnu#}
#else
foo = {#call pure fooNonGnu#}
#endif

main :: IO ()
main = print $ foo 0
