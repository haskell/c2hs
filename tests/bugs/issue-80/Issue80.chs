module Main where

import Control.Monad (forM_)

#ifdef DUMMY
  #include "rubbish.h"
#else
  #include "issue80.h"
#endif

{#enum foo as Foo {underscoreToCase} deriving (Eq, Show)#}

main :: IO ()
main = do
  forM_ [A, B, C, D] $ \v ->
    putStrLn $ show v ++ "=" ++ (show $ fromEnum v)
