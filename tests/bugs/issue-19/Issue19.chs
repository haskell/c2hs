module Main where

import Control.Monad

#include "issue19.h"

{#context prefix="enums"#}

{#enum enums1 as Enums1 {underscoreToCase}#}

{#enum enums2 as Enums2 {underscoreToCase} add prefix="TEST"#}

main :: IO ()
main  = do
  unless (1 == fromEnum One) $ putStrLn "1 /= One!!!"
  unless (5 == fromEnum TestFive) $ putStrLn "5 /= TestFive!!!"
  putStrLn "Did it!"
