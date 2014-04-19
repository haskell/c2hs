module Main where

import Control.Monad (forM_)

#include "issue79.h"

{#enum foo as Foo {underscoreToCase} deriving (Eq, Show)#}

main :: IO ()
main = do
  forM_ [A, B, C, D] $ \v ->
    putStrLn $ show v ++ "=" ++ (show $ fromEnum v)
