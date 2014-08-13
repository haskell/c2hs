{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface #-}
module Main where

import Control.Applicative

import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.Storable
import Foreign.Ptr

#include "issue93.h"

data Foo
data Bar = Bar Int Int

instance Storable Bar where
    sizeOf _ = {#sizeof bar_t #}
    alignment _ = {#alignof bar_t #}
    peek p = Bar
      <$> (fromIntegral <$> {#get bar_t.y #} p)
      <*> (fromIntegral <$> {#get bar_t.z #} p)
    poke p (Bar y z) =
         ({#set bar_t.y #} p $ fromIntegral y)
      *> ({#set bar_t.z #} p $ fromIntegral z)

{#pointer *foo_t as FooPtr -> Foo #}
{#pointer *bar_t as BarPtr -> Bar #}

{#fun unsafe mutate_foo as mutateFoo
  { `FooPtr'
  , with* `Bar'
  } -> `()' #}

main :: IO ()
main = putStrLn "OK"
