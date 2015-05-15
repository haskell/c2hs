module Main where

#include "issue38.h"

{#enum test_enum as TestEnum {underscoreToCase} deriving (Eq, Show)#}
{#fun enum_test {`TestEnum'} -> `TestEnum'#}

main :: IO ()
main = do
  res1 <- enum_test TestA
  res2 <- enum_test TestB
  res3 <- enum_test TestC
  case (res1, res2, res3) of
    (TestB, TestC, TestA) -> putStrLn "Enum OK"
    _                     -> putStrLn "Enum FAILED"
