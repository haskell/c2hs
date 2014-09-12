module Main where

check :: Bool
#if (C2HS_MIN_VERSION(0,18,2))
check = True
#else
check = False
#endif

main :: IO ()
main = print check
