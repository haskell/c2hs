module Main where

#include "issue113.h"

{#enum annoying as Annoying {0 as Zero} with prefix = "annoying"#}

main :: IO ()
main = putStrLn "OK"
