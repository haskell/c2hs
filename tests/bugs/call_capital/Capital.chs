module Main where

#include "capital.h"
main = do
  {# call C as ^ #}
  {# call c as c' #}
  {# call C as c'' #}
