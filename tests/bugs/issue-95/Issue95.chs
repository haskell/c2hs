module Main where

#include "issue95.h"

fooSize :: Int
fooSize = {# sizeof foo #}
