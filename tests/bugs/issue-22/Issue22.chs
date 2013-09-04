module Main where

import Foreign
import Foreign.C

#include "issue22.h"

{#pointer *struct_t as Struct#}
{#pointer *substruct_t as SubStruct#}

ptrToField :: Struct -> Ptr CChar
ptrToField p = p `plusPtr` {#offsetof struct_t->somefield#}

ptrToMember :: Struct -> SubStruct
ptrToMember p = p `plusPtr` {#offsetof struct_t->substruct#}

ptrToMemberPtr :: Struct -> Ptr SubStruct
ptrToMemberPtr p = p `plusPtr` {#offsetof struct_t->substruct_p#}

{#fun foo {`Int'} -> `Struct' return* #}

main :: IO ()
main = do
  p <- foo 2
  let fldp = ptrToField p
      subp = ptrToMember p
  subpp <- peek $ ptrToMemberPtr p
  s <- peekCString fldp
  subval <- {#get substruct_t.field#} subp
  subpval <- {#get substruct_t.field#} subpp
  putStrLn s
  print subval
  print subpval
