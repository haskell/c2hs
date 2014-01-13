module Main where

#include "issue7.h"

tst :: String
tst = "命令行"

main :: IO ()
main  = {#call foo#}
