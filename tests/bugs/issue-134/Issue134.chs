module Main where

#include <fcntl.h>

{# pointer *flock as ^ foreign newtype #}

main :: IO ()
main = putStrLn "OK"
