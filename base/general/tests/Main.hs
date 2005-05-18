module Main (main)
where

import FiniteMaps (FiniteMap, zeroFM, unitFM, listToFM, listToCombFM, joinFM, 
		   joinCombFM, sizeFM, addToFM, addToCombFM, delFromFM, diffFM,
		   intersectFM, intersectCombFM, mapFM, foldFM, filterFM, 
		   lookupFM, lookupDftFM, toListFM)


numMap :: FiniteMap Int String
numMap  = listToFM [(10, "zehn"), (2, "zwei"), (5, "fuenf"), (18, "achtzehn"),
		    (23, "dreiundzwanzig"), (8, "acht")]

nums = foldFM (\_ e t -> e:t) [] numMap

fibMap  :: Int -> FiniteMap Int Int
fibMap n  = listToFM [(i, fib i) | i <- [1..n]]
	    where
	      fib 0 = 1
	      fib 1 = 1
	      fib n = fib (n - 1) + fib (n - 2)

{- some debugging funs (don't work here, as they have to see into the abstract
   data type):
balDiff                    :: Ord k => FiniteMap k e -> [Int]
balDiff Leaf                = []
balDiff (Node _ _ _ sm gr)  = (abs (sizeFM sm - sizeFM gr) : balDiff sm) 
			      ++ balDiff gr

balRatio                    :: Ord k => FiniteMap k e -> [Int]
balRatio Leaf                = []
balRatio (Node _ _ _ sm gr) | sm_n == 0
			      || gr_n == 0 = (0 : balRatio sm) 
					     ++ balRatio gr
			    | sm_n > gr_n  = (sm_n `div` gr_n : balRatio sm) 
					     ++ balRatio gr
			    | otherwise    = (gr_n `div` sm_n : balRatio sm) 
					     ++ balRatio gr
					     where
					       sm_n = sizeFM sm
					       gr_n = sizeFM gr
-}

main :: IO ()
main  = do
          putStrLn "Test program for `FiniteMaps'"
          putStrLn "============================="
	  newline
          putStrLn "A finite map:"
	  print numMap
	  putStrLn "And its list of values:"
	  print nums
	  newline
	  putStrLn "Let's delete the key `18' from this map:"
	  print (delFromFM 18 numMap)
	  putStrLn "Next, a finite map of Fibonacci numbers:"
	  let fibs = fibMap 30
	  print fibs
	  putStrLn "And this map with all even occurrences removed \
		   \(should be 20):"
	  print (filterFM (const odd) fibs)
        where
	  newline = putStrLn ""
