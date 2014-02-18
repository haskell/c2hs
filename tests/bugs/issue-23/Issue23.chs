module Main where

import Foreign.C

#include "issue23.h"
#include "issue23x.h"

{#enum hello as Hello {underscoreToCase} deriving (Show)#}

{#fun hello_fn {`Int'} -> `Hello'#}

main :: IO ()
main = do
  res <- hello_fn 0
  putStrLn $ show res
