module Main where

#include "issue152.h"

f, g :: Int
f = {# sizeof a #}
g = {# sizeof s_a #}

main :: IO ()
main = putStrLn "OK"
