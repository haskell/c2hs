module Main where

import Control.Monad

#include "issue10.h"

check :: Int -> Int -> IO ()
check sz szexpect =
  putStrLn $ if sz == szexpect then "SAME"
             else ("DIFF: " ++ show sz ++ " vs. " ++ show szexpect)

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
  let sz5 = {# sizeof S5 #} :: Int
  sz5expect <- liftM fromIntegral {# call size_of_s5 #} :: IO Int
  check sz1 sz1expect
  check sz2 sz2expect
  check sz3 sz3expect
  check sz4 sz4expect
  check sz5 sz5expect
