-- To build, do						          -*-haskell-*-
--   % gcc -c structs.c
--   % ../c2hs structs.h Structs.chs
--   % ghc -fglasgow-exts '-#include<structs.h>' -o structs\
--         -i../lib -L../lib Structs.hs structs.o -lc2hs

import Monad (liftM, when)
import C2HS

newtype Point = Point {#type point#}

unPoint :: Point -> {#type point#}
unPoint (Point p) = p

makePoint     :: Int -> Int -> Point
makePoint x y  = Point ({#call fun make_point#} (cFromInt x) (cFromInt y))

main :: IO () 
main  = do
          val   <- liftM cToInt $ {#get _point.y#} $! unPoint pnt
          val'  <- liftM cToInt $ {#get point->y#} $! unPoint pnt
	  when (val /= val') $
	    error "val /= val': Panic!"
	  weird <- {#call make_weird#}
          val2  <- liftM cToInt $ {#get weird->x#} weird
          val3  <- liftM cToInt $ {#get weird->nested.z#} weird
          val4  <- liftM cToInt $ {#get weird->nested.pnt->y#} weird
          const nop $ {#set cpoint->col#} nullAddr 5 
		      -- only for seeing what is generated
          spacePtr <- {#call getSpacePtr#}
	  space <- liftM cToChar $ {#get *mychar#} spacePtr;
	  putStr (show val  ++ " & " ++ 
		  show val2 ++ " & " ++ 
		  show val3 ++ " & " ++ 
		  show val4 ++ " & " ++ 
		  show space ++ "\n")
	where
	  pnt   = makePoint 35 42
	  nop = return ()
