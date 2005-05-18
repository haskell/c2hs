module Main where

{-
Trace On:
joinCombFM: 
 left:   
  Ident "f4" (-1) 6758 (AttrsOnlyPos ("example.fl", 13, 13))
  Ident "f10" (-1) 792806 (AttrsOnlyPos ("example.fl", 10, 13))
 right:  
  Ident "f5" (-1) 6886 (AttrsOnlyPos ("example.fl", 16, 13))
  Ident "f6" (-1) 7014 (AttrsOnlyPos ("example.fl", 19, 26))
  Ident "f7" (-1) 7142 (AttrsOnlyPos ("example.fl", 21, 26))
 result: 
  Ident "f4" (-1) 6758 (AttrsOnlyPos ("example.fl", 13, 13))
  Ident "f7" (-1) 7142 (AttrsOnlyPos ("example.fl", 21, 26))
  Ident "f6" (-1) 7014 (AttrsOnlyPos ("example.fl", 19, 26))
  Ident "f5" (-1) 6886 (AttrsOnlyPos ("example.fl", 16, 13))
  Ident "f10" (-1) 792806 (AttrsOnlyPos ("example.fl", 10, 13))
Trace Off.
-}


import FiniteMaps (FiniteMap, listToFM, toListFM, joinFM)


left  = listToFM [(6758, ()), (792806, ())]
right = listToFM [(6886, ()), (7014, ()), (7142, ())]

result = joinFM left right

newline = putChar '\n'

sort      :: Ord a => [a] -> [a]
sort []    = []
sort (m:l) = (sort . filter (< m)) l ++ [m] ++ (sort . filter (>= m)) l 

sorted   :: Ord a => [a] -> Bool
sorted l  = sort l == l

main :: IO ()
main  = let
	  keys = toListFM result
	in
--        print ((toListFM . listToFM) [(6758, ()), (7142, ()), (7014, ()), (6886, ()), (792806, ())]) >>
	print "Test for `FiniteMaps': unordered bug"	>>
        print "===================================="	>>
	newline						>>
        print (keys)					>>
	if (sorted keys)
	then
	  print "OK!"
        else
	  print "ERROR: Above list is unordered!"

