module Main where

import Foreign.C

#include <wchar.h>

{#typedef wchar_t CWchar#}
{#default in `String' [wchar_t *] withCWString* #}
{#default out `String' [wchar_t *] peekCWString* #}
{#fun wcscmp {`String', `String'} -> `Int'#}
{#fun wcscat {`String', `String'} -> `String'#}

main :: IO ()
main = do
  wcscmp "abc" "def" >>= print . signum
  wcscat "abc" "def" >>= putStrLn
