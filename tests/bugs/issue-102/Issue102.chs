module Main where

#include <stdio.h>
#include <fcntl.h>

{#pointer *FILE as File foreign finalizer fclose newtype#}

{#fun fopen as ^ {`String', `String'} -> `File'#}
{#fun fileno as ^ {`File'} -> `Int'#}

{#fun variadic fprintf[int] as fprinti
    {`File', `String', `Int'} -> `()'#}
{#fun variadic fprintf[int, int] as fprinti2
    {`File', `String', `Int', `Int'} -> `()'#}
{#fun variadic fprintf[const char *] as fprints
    {`File', `String', `String'} -> `()'#}

{#fun variadic printf[int] as printi {`String', `Int'} -> `()'#}
{#fun variadic printf[int, int] as printi2 {`String', `Int', `Int'} -> `()'#}
{#fun variadic printf[const char *] as prints {`String', `String'} -> `()'#}

{#enum define FCntlAction {F_GETLK as GetLock, F_SETLK as SetLock}
          deriving (Eq, Ord, Show)#}
{#enum define FCntlLockState
    {F_RDLCK as ReadLock, F_WRLCK as WriteLock, F_UNLCK as Unlocked}
          deriving (Eq, Ord, Show)#}
{#pointer *flock as FLock foreign newtype#}
{#fun variadic fcntl[struct flock *] as
         f_get_lock {`Int', `Int', +} -> `FLock'#}
{#fun variadic fcntl[struct flock *] as
         f_set_lock {`Int', `Int', `FLock'} -> `Int'#}

main :: IO ()
main = do
  f <- fopen "issue-102.txt" "w"
  printi "TST 1: %d\n" 1234
  printi2 "TST 2: %d %d\n" 13 47
  prints "TST 3: %s\n" "testing"
  fprinti f "TST 1: %d\n" 1234
  fprinti2 f "TST 2: %d %d\n" 13 47
  fprints f "TST 3: %s\n" "testing"
  -- -- This part of the test is broken:
  -- fd <- fileno f
  -- flck <- get_lock fd
  -- withFLock flck $ \lck -> do
  --   typ <- {#get flock.l_type#} lck
  --   print (toEnum $ fromIntegral typ :: FCntlLockState)
  -- -- Andreas Abel, 2022-02-05:
  -- -- The last line fails with:
  -- -- stderr: Issue102: FCntlLockState.toEnum: Cannot match 0
  -- -- It seems that typ==0 which is not a valid lock state,
  -- -- maybe some exceptional value.

get_lock :: Int -> IO FLock
get_lock fd = f_get_lock fd (fromEnum GetLock)

set_lock :: Int -> FLock -> IO Int
set_lock fd lck = f_set_lock fd (fromEnum SetLock) lck
