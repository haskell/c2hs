module Main where

#include "issue65.h"

const1 :: Int
const1 = {#const CONST1#}

const2 :: Integer
const2 = {#const CONST2#}

main :: IO ()
main = print const1 >> print const2
