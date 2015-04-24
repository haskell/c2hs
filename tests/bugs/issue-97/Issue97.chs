-- Main.chs
{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

{#import Issue97A#}
import Foreign
import Foreign.C.Types
import System.IO.Unsafe (unsafePerformIO)

#include "issue97.h"

{#fun pure foo_x as fooX { `FooPtr' } -> `Int' #}

main :: IO ()
main = allocaBytes {#sizeof foo_t #} $ \fooPtr -> do
    {#set foo_t.x #} fooPtr 42
    print $ fooX fooPtr
