module Main where

#include "Capital.h"
main = do
  {# call C as ^ #}
  {# call c as c' #}
  {# call C as c'' #}
