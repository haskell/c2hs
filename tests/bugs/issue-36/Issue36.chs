module Main where

#include "issue36.h"

{#pointer *hit_int as OK_Hit -> Hit Int#}
{#pointer *hit_double as OK_Hit -> Hit Double#}

main :: IO ()
main = return ()
