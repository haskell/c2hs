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
makePoint x y  = Point ({#call fun make_point#} (cIntConv x) (cIntConv y))

pointSize :: Int
pointSize  = {#sizeof point#}

bar = {#sizeof SDL_Event#}  -- regression test

main :: IO () 
main  = do
          val   <- liftM cIntConv $ {#get _point.y#} $! unPoint pnt
          val'  <- liftM cIntConv $ {#get point->y#} $! unPoint pnt
	  when (val /= val') $
	    error "val /= val': Panic!"
	  weird <- {#call make_weird#}
          val2  <- liftM cIntConv $ {#get weird->x#} weird
          val3  <- liftM cIntConv $ {#get weird->nested.z#} weird
          val4  <- liftM cIntConv $ {#get weird->nested.pnt->y#} weird
          const nop $ {#set cpoint->col#} nullPtr 5 
		      -- only for seeing what is generated
          spacePtr <- {#call getSpacePtr#}
	  space <- liftM castCCharToChar $ {#get *mychar#} spacePtr;
	  -- bitfields
	  bitStructPtr <- {#call get_bit_struct#}
	  {#set bit_struct.bit#} bitStructPtr 0
	  bit          <- {#get bit_struct.bit#} bitStructPtr
	  when (bit /= 0) $
	    error "bit /= 0: Panic!"
	  smallInt     <- {#get bit_struct.very_small_int#} bitStructPtr
	  when (smallInt /= -1) $
	    error "smallInt /= -1: Panic!"
	  --
	  putStr (show val  ++ " & " ++ 
		  show val2 ++ " & " ++ 
		  show val3 ++ " & " ++ 
		  show val4 ++ " & " ++ 
		  show space ++ "\n")
	where
	  pnt   = makePoint 35 42
	  nop = return ()
