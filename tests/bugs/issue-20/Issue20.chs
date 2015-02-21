module Main where

import Foreign
import Foreign.C
import Foreign.C.String

#include <wchar.h>
#include "issue20.h"

{#default size_t CSize in=id out=id#}
{#fun foo {`Int'} -> `CSize'#}

{#default wchar_t CWChar ptr_in=withCWString* ptr_out=peekCWString*#}
{#fun wcscmp {`CWString', `CWString'} -> `Int'#}

main :: IO ()
main = do
  s1 <- foo 1
  s4 <- foo 4
  print $ s4 `div` s1
