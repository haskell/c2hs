-- -*-haskell-*-
import C2HS

{#pointer string as MyCString foreign newtype#}

data Point = Point {
	       x :: Int,
	       y :: Int
	     }

{#pointer *Point as CPoint foreign -> Point#}

makeCPoint     :: Int -> Int -> CPoint
makeCPoint x y  = {#call unsafe make_point#} (cIntConv x) (cIntConv y)

main = print 42
