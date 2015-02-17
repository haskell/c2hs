module Main where

#include "issue116.h"

{#enum test_enum as TestEnum {underscoreToCase} omit (TOTAL_ENUM_COUNT)
    deriving (Eq, Show)#}

-- Force name overlap: causes compilation failure if "omit" in enum
-- hook doesn't work.
data Check = TotalEnumCount
           | Dummy

main :: IO ()
main = print (fromEnum E1, fromEnum E2, fromEnum E3)
