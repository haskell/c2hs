module Main where

import Foreign.Storable
import Foreign.Ptr

#include "issue140.h"


{#pointer *ptr1 as Ptr1 foreign newtype#}
{#pointer *ptr2 as Ptr2 foreign newtype#}
{#pointer *ptr3 as Ptr3 foreign newtype#}

instance Storable Ptr2 where
  sizeOf _ = 8
  alignment _ = 1
  peekElemOff p i = peekElemOff (castPtr p) i
  pokeElemOff p i x = pokeElemOff (castPtr p) i x

{#fun f1 as ^ {+, `Int'} -> `Ptr1'#}
{#fun f2 as ^ {+S, `Int'} -> `Ptr2'#}
{#fun f3 as ^ {+16, `Int'} -> `Ptr3'#}


main :: IO ()
main = do
  p1 <- f1 123
  p2 <- f2 456
  p3 <- f3 789
  chk1 <- withPtr1 p1 {#get ptr1->a#}
  chk2 <- withPtr2 p2 {#get ptr2->a#}
  chk3 <- withPtr3 p3 {#get ptr3->a#}
  print chk1
  print chk2
  print chk3
