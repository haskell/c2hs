module Main where

#include "issue15.h"

{#enum Tst as ^ {underscoreToCase} deriving (Eq, Show)#}

main :: IO ()
main = print kClippingCreator
