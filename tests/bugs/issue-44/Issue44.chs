module Main where

#include "issue44.h"

{#pointer *foo as ^ foreign newtype#}

main :: IO ()
main = putStrLn "dummy"
