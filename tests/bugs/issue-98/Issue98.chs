module Main where

#include "issue98.h"

{#fun pure identichar  as ^ { `Char' } -> `Char' #}
{#fun pure identiuchar as ^ { `Char' } -> `Char' #}
{#fun pure identischar as ^ { `Char' } -> `Char' #}

main :: IO ()
main = print $ map ($ 'A') [identichar, identiuchar, identischar]
