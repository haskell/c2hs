module Main where

import Control.Monad
import Foreign.C.Types
import Foreign.Marshal.Utils

#include "issue131.h"

{#fun f1 as ^ {`Int', `Bool'} -> `Int'#}
{#fun f2 as ^ {`Int'} -> `Bool'#}

main :: IO ()
main = do
  f1 4 True >>= print
  f1 4 False >>= print
  f2 4 >>= print
  f2 0 >>= print
