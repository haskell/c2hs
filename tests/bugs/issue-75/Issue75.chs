module Main where

import Foreign
import Foreign.C

{#context prefix="chk"#}

#include "issue75.h"

data TstStruct = TstStruct { a :: Int, b :: Int }
{#pointer *TST as TstPtr -> TstStruct#}
{#fun make_tst as ^ {} -> `TstPtr'#}
{#fun tst as ^ {`TstPtr'} -> `Int'#}

main :: IO ()
main = do
  let size = {#sizeof TST#}
      align = {#alignof TST#}
  s <- makeTst
  res <- tst s
  putStrLn $ show size
  putStrLn $ show align
  putStrLn $ if res == 5 then "OK" else "BAD"

