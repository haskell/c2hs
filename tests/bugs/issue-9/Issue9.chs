module Main where

#include "issue9.h"

main :: IO ()
main = do
  putStrLn $ "PTA:" ++ show ({# sizeof pointer_to_array #} :: Int)
  putStrLn $ "AOP:" ++ show ({# sizeof array_of_pointers #} :: Int)
  print (({# sizeof inner_t #}, {# sizeof outer_t #}) :: (Int, Int))
  print ({# sizeof ok_outer_t #} :: Int)
  putStrLn "OK"
