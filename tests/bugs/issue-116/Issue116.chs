module Main where

import Control.Monad
import Foreign
import Foreign.C

#include "issue116.h"

{#enum test_enum as TestEnum {underscoreToCase} omit TOTAL_ENUM_COUNT
    deriving (Eq, Show)#}

data Check = TotalEnumCount
           | Dummy

main :: IO ()
main = do
  res1 <- enum_test E1
  res2 <- enum_test E2
  res3 <- enum_test E3
  case (res1, res2, res3) of
    (E2, E3, E1) -> putStrLn "Enum OK"
    _            -> putStrLn "Enum FAILED"
