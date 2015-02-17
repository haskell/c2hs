module Main where

import Foreign
import Foreign.C

#include "issue37.h"

{#fun f1 {`Int'} -> `Int'#}

{#fun f2 {`Float'} -> `Float'#}

main :: IO ()
main = do
  tst1 <- f1 7
  tst2 <- f2 23
  putStrLn $ if tst1 == 14 then "SAME" else "DIFF"
  putStrLn $ if tst2 == 69 then "SAME" else "DIFF"
