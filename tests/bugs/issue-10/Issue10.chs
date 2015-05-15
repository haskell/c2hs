module Main where

import Control.Monad

#include "issue10.h"

main :: IO ()
main = do
  let sz1 = {# sizeof S1 #} :: Int
  sz1expect <- liftM fromIntegral {# call size_of_s1 #} :: IO Int
  let sz2 = {# sizeof S2 #} :: Int
  sz2expect <- liftM fromIntegral {# call size_of_s2 #} :: IO Int
  let sz3 = {# sizeof S3 #} :: Int
  sz3expect <- liftM fromIntegral {# call size_of_s3 #} :: IO Int
  let sz4 = {# sizeof S4 #} :: Int
  sz4expect <- liftM fromIntegral {# call size_of_s4 #} :: IO Int
  putStrLn $ if sz1 == sz1expect then "SAME" else "DIFF"
  putStrLn $ if sz2 == sz2expect then "SAME" else "DIFF"
  putStrLn $ if sz3 == sz3expect then "SAME" else "DIFF"
  putStrLn $ if sz4 == sz4expect then "SAME" else "DIFF"
