-- To build, do						          -*-haskell-*-
--   {-% gcc -c marsh.c-}
--   % ../c2hs marsh.h Marsh.chs
--   % ghc -fglasgow-exts '-#include<marsh.h>' -o marsh\
--         -i../lib -L../lib Marsh.hs {-marsh.o-} -lc2hs

import C2HS

main :: IO ()
main  = do
	  mem <- stdAddr "Hello World!\n"
	  str <- addrStdKeep mem
	  free mem
	  putStr str

	  (mem, len) <- listToAddrWithLen ([5, 3, 7] :: [CInt])
	  l <- addrWithLenToList mem len
	  free mem
	  putStr $ show (l :: [CInt]) ++ "\n"
