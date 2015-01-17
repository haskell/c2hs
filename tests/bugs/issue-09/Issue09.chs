module Main where

#include "issue09.h"

main :: IO ()
main = do
  putStrLn $ "PTA:" ++ show {# sizeof pointer_to_array #}
  putStrLn $ "AOP:" ++ show {# sizeof array_of_pointers #}
  print ({# sizeof inner_t #}, {# sizeof outer_t #})
  print {# sizeof ok_outer_t #}
  putStrLn "OK"
