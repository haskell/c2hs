module Main where

#include "issue134.h"

{# pointer *tst as ^ foreign newtype #}

main :: IO ()
main = putStrLn "OK"
