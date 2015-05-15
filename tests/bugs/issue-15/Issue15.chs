module Main where

import Numeric
import Data.Char

#include "issue15.h"

{#enum Tst as ^ {underscoreToCase} deriving (Eq, Show)#}

main :: IO ()
main = do
  tst <- {#call tst_val#}
  let chk1 = showIntAtBase 16 intToDigit tst ""
      chk2 = showIntAtBase 16 intToDigit (fromEnum Kclippingcreator) ""
  print $ chk1 == chk2
