module Main where
import Foreign.C.Types(CULLong)
#include "issue242.h"

{# fun echoCULLong as ^ {`CULLong'} -> `CULLong' #}

main :: IO ()
main = do
  let input :: CULLong
      input = 1
  output <- echoCULLong input
  putStrLn (show output)
