module Main where

#include "issue127.h"

{#fun tst as ^ {`Int'} -> `Bool'#}

main :: IO ()
main = do
  tst 5 >>= print
  tst (-2) >>= print
