-- -*-haskell-*-
import Monad
import C2HS

{#pointer string as MyCString foreign newtype#}

cconcat       :: MyCString -> MyCString -> IO MyCString
cconcat s1 s2  = do
  ptr <- {#call concat as _concat#} s1 s2
  liftM MyCString $ newForeignPtr ptr (free ptr)

data Point = Point {
	       x :: Int,
	       y :: Int
	     }

{#pointer *Point as CPoint foreign -> Point#}

-- this is just to exercise some more paths in GenBind.hs
{#pointer *_Point as C_Point foreign -> Point#}
{#pointer PointPtr#}

makeCPoint     :: Int -> Int -> IO CPoint
makeCPoint x y  = do
  ptr <- {#call unsafe make_point#} (cIntConv x) (cIntConv y)
  newForeignPtr ptr (free ptr)

transCPoint :: CPoint -> Int -> Int -> IO CPoint
transCPoint pnt x y = do
  ptr <- {#call unsafe trans_point#} pnt (cIntConv x) (cIntConv y)
  newForeignPtr ptr (free ptr)

main = print 42
