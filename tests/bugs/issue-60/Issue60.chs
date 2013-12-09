module Main where

import Foreign.C

#include "issue60.h"

main :: IO ()
main = putStrLn "OK"
