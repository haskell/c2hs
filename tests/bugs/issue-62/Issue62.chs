module Main where

import Foreign.C
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable

#include "issue62.h"

peekToInt :: Ptr CInt -> IO Int
peekToInt p = 
    peek p >>= return . fromIntegral
    
{# fun f1
    { `Int' -- ^ This is a multiline
            -- comment for
            -- para1
    , `Int'
    , `Int' -- ^ comment for para3
    } -> `Int' -- ^ multiline
              -- comment for
              -- result
 #}

{# fun f2
    { `Int'
    , alloca- `Int' peekToInt*  -- ^ comment
                                -- won't appear 
    , alloca- `Int' peekToInt* -- ^ won't appear
    } -> `Int' -- ^ The only comment for result
 #}

main :: IO ()
main = return ()
