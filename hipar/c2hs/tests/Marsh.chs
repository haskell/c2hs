-- To build, do						          -*-haskell-*-
--   {-% gcc -c marsh.c-}
--   % ../c2hs marsh.h Marsh.chs
--   % ghc -fglasgow-exts '-#include<marsh.h>' -o marsh\
--         -i../lib -L../lib Marsh.hs {-marsh.o-} -lc2hs

import C2HS

main :: IO ()
main  = do
	  mem <- newCString "Hello World!\n"
	  str <- peekCString mem
	  free mem
	  putStr str

	  let l   = [5, 3, 7] :: [CInt]
	      len = length l
	  mem <- newArray l
	  l <- peekArray len mem
	  free mem
	  putStr $ show l ++ "\n"
