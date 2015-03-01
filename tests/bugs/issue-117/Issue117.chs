module Main where

import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import System.IO.Unsafe

#include "issue117.h"

{#pointer *coord_t as CoordPtr foreign finalizer free_coord newtype#}

{#fun pure make_coord as makeCoord {`Int', `Int'} -> `CoordPtr'#}
{#fun pure coord_x as coordX {%`CoordPtr', `Int'} -> `Int'#}

main :: IO ()
main = do
  let c = makeCoord 5 6
  let x = coordX c 0
  print x
