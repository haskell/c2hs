module Main where

import Foreign
import Foreign.C

{#context prefix="chk"#}

#include "issue75.h"

data TstStruct = TstStruct { a :: Int }
{#pointer *TST as TstPtr -> TstStruct#}
{#fun make_tst as ^ {} -> `TstPtr'#}

main :: IO ()
main = do
  s <- makeTst
  aval <- {#get CHK_TST.a#} s
  putStrLn $ show aval

