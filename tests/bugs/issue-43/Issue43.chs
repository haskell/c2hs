module Main where

import Control.Monad (forM_)

#include "issue43.h"

{#enum Test1 {underscoreToCase} deriving (Eq, Show)#}

{#enum ANON_A as Anon {underscoreToCase} deriving (Eq, Show)#}

main :: IO ()
main = do
  forM_ [Test1A, Test1B, Test1C, Test1D] $ \v ->
    putStrLn $ show v ++ "=" ++ (show $ fromEnum v)
  forM_ [AnonA, AnonB, AnonC, AnonD] $ \v ->
    putStrLn $ show v ++ "=" ++ (show $ fromEnum v)
