module Main where

#include "issue133.h"

{#pointer tdptst as VoidTest1#}
{#pointer *tdtst as VoidTest2#}

main :: IO ()
main = putStrLn "OK"
