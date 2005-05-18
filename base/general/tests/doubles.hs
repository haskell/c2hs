module Main (main) where

-- This module test whether joining two maps with equal keys works correctly.

import FiniteMaps (FiniteMap, unitFM, toListFM, joinFM, joinCombFM, addToFM)


main :: IO ()
main = 
 let
     map1 = addToFM 1 () (unitFM 2 ())
     map2 = addToFM 1 () (unitFM 3 ())
--     finalmap = joinCombFM const map1 map2
     finalmap = joinFM map1 map2
 in
     putStr (maptostring finalmap)


maptostring :: (FiniteMap Int ()) -> String
maptostring fmap = 
  let
      list = toListFM fmap
  in
      concat (map (show . fst) list) ++ "\n"
