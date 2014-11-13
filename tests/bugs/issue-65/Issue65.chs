module Main where

#include "issue65.h"

const1 :: Int
const1 = {#const CONST1#}

const2 :: Double
const2 = {#const CONST2#}

const3 :: String
const3 = {#const CONST3#}

main :: IO ()
main = print const1 >> print const2 >> print const3
