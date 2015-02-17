module Main where

import Control.Monad
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types

#include <string.h>
#include <stdlib.h>
#include <math.h>

-- This is for testing marshalling of C... types, e.g. CInt, etc.
{#fun strcmp as ^ {`CString', `CString'} -> `CInt'#}
{#fun setenv as ^ {`String', `String', `Int'} -> `Int'#}
{#fun getenv as ^ {`String'} -> `CString'#}
{#fun sin as hsin {`Double'} -> `Double'#}
{#fun sin as csin {`CDouble'} -> `CDouble'#}
{#fun malloc as ^ {`CULong'} -> `Ptr ()'#}
{#fun free as ^ {`Ptr ()'} -> `()'#}
{#fun strcpy as ^ {`CString', `CString'} -> `()'#}

main :: IO ()
main = do
  let s1 = "abc" ; s2 = "def" ; s3 = "def"
  res1 <- withCString s1 $ \cs1 ->
    withCString s2 $ \cs2 -> strcmp cs1 cs2
  res2 <- withCString s2 $ \cs2 ->
    withCString s3 $ \cs3 -> strcmp cs2 cs3
  print (res1 < 0, res2 == 0)
  void $ setenv "TEST_VAR" "TEST_VAL" 1
  h <- getenv "TEST_VAR"
  peekCString h >>= putStrLn
  cx <- csin 1.0
  print (round (10000 * cx) :: Integer)
  hx <- hsin 1.0
  print (round (10000 * hx) :: Integer)
  let s = "TESTING"
  p <- malloc $ fromIntegral $ length s + 1
  let ps = castPtr p :: CString
  cs <- newCString s
  strcpy ps cs
  res <- peekCString ps
  putStrLn res
  free p
