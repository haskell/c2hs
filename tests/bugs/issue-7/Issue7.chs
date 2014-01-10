module Main where

#include "issue7.h"

main :: IO ()
main  = {#call foo#}
