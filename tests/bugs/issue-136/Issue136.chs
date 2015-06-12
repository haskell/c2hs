{-# LANGUAGE EmptyDataDecls,
    ForeignFunctionInterface #-}

{- |
This will break things if you're not careful about comment parsing...
-- Hmmm...
-}

-- And so will this -}

module Main where

import Control.Applicative ( (<$>)
                           , (<*>)
                           , (*>))
import Foreign.Marshal.Utils
import Foreign.Storable

#include "issue136.h"

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
