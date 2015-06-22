module Main where

#include "issue36.h"

data Hit1 a = Hit1 a
data Hit2 a b = Hit2 a b

{#pointer *hit_int as HitEg1 -> Hit1 Int#}
{#pointer *hit_double as HitEg2 -> Hit1 Double#}
{#pointer *hit_int as HitEg3 -> `Hit2 Int ()'#}
{#pointer *hit_double as HitEg4 -> `Hit2 Double [Int]'#}

main :: IO ()
main = return ()
