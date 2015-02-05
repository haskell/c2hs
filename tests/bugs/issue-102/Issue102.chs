module Main where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

#include <stdio.h>
#include <fcntl.h>

{#fun variadic printf[int] as printi {`String', `Int'} -> `()'#}
{#fun variadic printf[int, int] as printi2 {`String', `Int', `Int'} -> `()'#}
{#fun variadic printf[const char *] as prints {`String', `String'} -> `()'#}

{#enum define FCntlAction {F_GETLK as GetLock, F_SETLK as SetLock}
          deriving (Eq, Ord)#}
{#pointer *flock as FLock#}
{#fun variadic fcntl[struct flock *] as
         f_get_lock {`Int', `Int', `FLock'} -> `Int'#}
{#fun variadic fcntl[struct flock *] as
         f_set_lock {`Int', `Int', `FLock'} -> `Int'#}

main :: IO ()
main = do
  printi "TST 1: %d\n" 1234
  printi2 "TST 2: %d %d\n" 13 47
  prints "TST 3: %s\n" "testing"

get_lcok, set_lock :: Int -> FLock -> IO Int
get_lock fd lck = f_set_lock fd (fromEnum GetLock) lck
set_lock fd lck = f_set_lock fd (fromEnum SetLock) lck
