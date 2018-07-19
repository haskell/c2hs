module Main where

#include "issue230.h"

import Control.Monad (liftM)
import Foreign.C

cIntConv :: CInt-> Int
cIntConv = fromIntegral

cDblConv :: CDouble -> Double
cDblConv = realToFrac

main :: IO ()
main  = do
  test1 <- {#call make_test1#}
  val1A <- liftM cIntConv $ {#get test1->a#} test1
  val1B <- liftM cIntConv $ {#get test1->b#} test1
  val1C <- liftM cIntConv $ {#get test1->c#} test1
  val1D <- liftM cDblConv $ {#get test1->d#} test1

  test2 <- {#call make_test2#}
  val2A <- liftM cIntConv $ {#get test2->a#} test2
  val2B <- liftM cIntConv $ {#get test2->b#} test2
  val2C <- liftM cIntConv $ {#get test2->c#} test2
  val2D <- liftM cDblConv $ {#get test2->d#} test2

  putStrLn (show val1A)
  putStrLn (show val1B)
  putStrLn (show val1C)
  putStrLn (show val1D)
  putStrLn (show val2A)
  putStrLn (show val2B)
  putStrLn (show $ val2C /= 7)
  putStrLn (show val2D)

  return ()

