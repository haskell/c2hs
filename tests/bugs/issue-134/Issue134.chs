module Main where

#include "issue134.h"

{#pointer *tsttag as ^ foreign newtype#}

main :: IO ()
main = putStrLn "OK"
